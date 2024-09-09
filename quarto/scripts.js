function modifyMainDiv() {
  console.log('blue');
  const mainDiv = document.getElementById('quarto-document-content');
  if (mainDiv) {
    //mainDiv.classList.remove('column-body');
    //mainDiv.classList.add('column-page-right');
    // You can add more class modifications here
  }
}

// This function will be called by the script in the Quarto document
modifyMainDiv();