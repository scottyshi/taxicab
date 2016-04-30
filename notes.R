#NOTES 4/27/16

#dividing the tasks between us:
#offline matching: using hard data
#online matching: require more statistics and confidence intervals...

#NEW TASKS
  #predict where customers will pop up at any given time. 
  #Try to predict for times that have a denser amount of customers
      #learning set -- take some % of learning set randomly ~ 80%
      # -- i think that certain amount of customers will pop up in this region
      # take another % of learning set randomly
      # -- i think that this NEW amount of customer will pop up in this region
      # repeat this around 100 times -- this will give us a range of values
      # I want to give a range that 95% of all possible values fit in there.
        # OPTIONS TO EVALUATE POSSIBLE VALUES
        # Bradley Efron -- jacknife
        # Larry Wasserman
        # interval of confidence?
  #complete bipartite matching 