  
  
  
  var sample_size= document.getElementById("sample_size").value;
   var sdev= document.getElementById("sdev").value;
   var degf= document.getElementById("degf").value;
   var interv= document.getElementById("interv").value;
   var lambda= document.getElementById("lambda").value;
   var omittedvar= document.getElementById("omittedvar").value;
   var input2=1;
   
      	 function myFunction() {
     	 	 input2=input2+1;
    var distribution_chosen = document.getElementById("selectdistribution").value;
      	 	 Shiny.onInputChange("simulation", input2);  
      	 	 Shiny.onInputChange("distribution_chosen", distribution_chosen);  
        Shiny.onInputChange("sample_size", sample_size);  
        Shiny.onInputChange("sdev", sdev);  
        Shiny.onInputChange("degf", degf);  
        Shiny.onInputChange("interv", interv);  
        Shiny.onInputChange("lambda", lambda);  
        Shiny.onInputChange("omittedvar", omittedvar);  
                    }