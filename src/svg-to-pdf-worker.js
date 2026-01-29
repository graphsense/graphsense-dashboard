import { jsPDF } from 'jspdf'
self.onmessage = function(e) {
  let { imgData, contentWidth, contentHeight, filename } = e.data;
      // Create PDF with dimensions matching content
  const pdfWidth = contentWidth;
  const pdfHeight = contentHeight;
  const orientation = pdfWidth > pdfHeight ? 'landscape' : 'portrait';
  
  const pdf = new jsPDF({
    orientation: orientation,
    unit: 'px',
    format: [pdfWidth, pdfHeight],
    compress: true
  });

  pdf.addImage(imgData, 'PNG', 0, 0, pdfWidth, pdfHeight, undefined, 'FAST');
  
  // Save the PDF
  const pdfBlob = pdf.output('blob');
  console.log('pdfBlob', pdfBlob)
  self.postMessage({ pdfBlob, filename });
};
