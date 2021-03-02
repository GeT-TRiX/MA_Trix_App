shinyjs.enrichr = function(params) {
	var defaultParams = {
	list : null,
    popup : true,
    description : 'MyDescription'
  };
	options = shinyjs.getParams(params, defaultParams);
	
	alert('Sending gene list to Enrich web page. \nNote 1: if the page is blank at the first submission, please click again on `Submit to Enrichr` button. \nNote 2: Allow browser pop-up window when prompted ');
	

	if (typeof options.list === 'undefined') {
        alert('No genes defined.');
    }
    var description  = options.description || '',
    	popup = options.popup || true,
    	form = document.createElement('form'),
    	listField = document.createElement('input'),
    	descField = document.createElement('input');
  
    form.setAttribute('method', 'post');
    form.setAttribute('action', 'https://maayanlab.cloud/Enrichr/enrich');
    if (popup) {
        form.setAttribute('target', '_blank');
    }
    form.setAttribute('enctype', 'multipart/form-data');

    listField.setAttribute('type', 'hidden');
    listField.setAttribute('name', 'list');
    listField.setAttribute('value', options.list);
    form.appendChild(listField);

    descField.setAttribute('type', 'hidden');
    descField.setAttribute('name', 'description');
    descField.setAttribute('value', description);
    form.appendChild(descField);

    document.body.appendChild(form);
    form.submit();
    document.body.removeChild(form);
}
