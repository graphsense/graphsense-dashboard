import { jsPDF } from 'jspdf'
self.onmessage = function(e) {
  let { imgDataUrl, width, height, filename } = e.data;
  console.log('imgDataUrl', imgDataUrl)
      // Create PDF with dimensions matching content
  const aspect_ratio = width / height
  const max_dimension = 14400

  if(width > max_dimension || height > max_dimension) {
    if(width > height) {
        width = max_dimension
        height = width / aspect_ratio
    } else {
        height = max_dimension
        width = height * aspect_ratio
    }
  }
  console.log('topdf width/height', width, height)
  const orientation = width > height ? 'landscape' : 'portrait';

  const pdf = new jsPDF({
    orientation: orientation,
    unit: 'px',
    format: [width, height],
    compress: true,
    hotfixes: ["px_scaling"]
  });

  pdf.addImage(imgDataUrl, 'PNG', 0, 0, width, height, undefined, 'FAST');
  URL.revokeObjectURL(imgDataUrl)
  
  // Save the PDF
  const pdfBlob = pdf.output('blob');
  console.log('pdfBlob', pdfBlob)
  self.postMessage({ pdfBlob, filename });
};
