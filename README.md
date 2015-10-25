# MortSingleSpp

Input:
  SidePanel:
    
    Single species name selected from a pull down list.  Default: first species from list
    
    Amount of dbh categories selected from numeric input.  Default: 3
    
    Array of dbh categories selected from a dynamically generated series of n numeric inputs 
    where n = Amount of dbh categories.  Default: n*100
  MainPanel:
  
    Array of survey pairs to plot selected from a checkbox group.  Default all selected
    
    X Axis selected from a numeric input.  Default: Dbh category n + 100
    
    Y axis selected from a numeric input.  Default: Highest point in graph + 0.05
    
Output:

    A line graph of the mortality data of the species utilizing the dbh categories.
    
    Multiple line graphs corresponding to each survey pair, chosen by the checkbox group, may be overlaid.  
    Each survey pair will have it's own corresponding color.
  
