/*
   Authors: Gleamerlyn Prasetya
   (c) 2013 
*/
function colapseContent(me){
  
  //Get id of content and button
  var content = me.id + '_content'
  var button = me.id + '_button'
  
  //Debugging
  //console.info(">>" + content) ;
  
  if (document.getElementById(content).getAttribute("class") == "folderContent") {
  	//Set content visibility
    document.getElementById(content).setAttribute("class","folderContentOpen");
    console.info(">>" + (document.getElementById(content).getAttribute("class")));
	
	//Set button text
    document.getElementById(button).innerHTML = '[-] Hide ';
  }
  else{
  	//Set content visibility
    document.getElementById(content).setAttribute("class","folderContent");
	//Set button text	
    document.getElementById(button).innerHTML = '[+] Show ';
  };
  
};
