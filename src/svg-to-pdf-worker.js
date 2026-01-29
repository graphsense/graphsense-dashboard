import { jsPDF } from 'jspdf'
self.onmessage = function(e) {
  let { svgData, contentWidth, contentHeight, filename, bgColor } = e.data;
  console.log('svgData', svgData)
  //const svgDataBase64 = btoa(svgData);
  //console.log('svgDataBase64', svgDataBase64)
  const pixelScaleFactor = 4;
  const width = contentWidth * pixelScaleFactor;
  const height = contentHeight * pixelScaleFactor;

  const canvas = new OffscreenCanvas(width, height);
  const ctx = canvas.getContext("2d");

  const blobSvgData = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
  console.log('blob svgData', blobSvgData)
  const url = URL.createObjectURL(blobSvgData);
  console.log('url svgData', url)
  /*
  svgData = svgData.replace(/"/g, '\'')
        .replace(/%/g, '%25')
        .replace(/#/g, '%23')       
        .replace(/{/g, '%7B')
        .replace(/}/g, '%7D')         
        .replace(/</g, '%3C')
        .replace(/>/g, '%3E')

        .replace(/\s+/g,' ')

  const dataUri = "data:image/svg+xml," + (svgData);
  console.log(dataUri)


    */
  fetch(url)
    .then(res => res.blob())
    .then(blob => { console.log('blob', blob); const bm = createImageBitmap(blob, {type:'image/svg+xml;charset=utf-8'}); console.log('bm', bm); return bm})
  //createImageBitmap(blobSvgData)
    .then(imageBitmap => {
      console.log('bitmap')
      try {
        ctx.fillStyle = bgColor;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(imageBitmap, 0, 0, width, height);

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

        // Use PNG for better quality, with compression
        const imgData = canvas.toDataURL('image/png');
        pdf.addImage(imgData, 'PNG', 0, 0, pdfWidth, pdfHeight, undefined, 'FAST');

        // Return the PDF blob to the main thread
        const pdfBlob = pdf.output('blob');
        self.postMessage({ pdfBlob, filename });
      } catch (e) {
        self.postMessage({ error: 'PDF generation failed', details: e });
      }
    })
};
