Adding a new R script to the project:

 1. copy scripts/template.R to create a new script::
    cp scripts/template.R scripts/newscript.R

 2. define new inputs and output in params.ini if necessary    

[outfiles]

newtarget = outputOfNewscript.rda 


 3. add a new action to SConstruct::
    
newoutput = env.RScript(
    target = '$newtarget',
    source = Flatten(['scripts/newscript.R',
                      '$constants'])
    )


4. if the output is used in the final paper, add it to paper_depends

5. create an alias if you want (add to targetnames)
