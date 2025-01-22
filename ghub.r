## eval following to create a new private repo named "test" on github

## M-x eval-region
(ghub-post "/user/repos"
           '(
             (name . "fecal-thresholds")
             (private . t)
             )
           )



Configure master
 d branch.master.description unset
 u branch.master.merge       refs/heads/master
   branch.master.remote      origin
 r branch.master.rebase      [true|false|default:false]
 p branch.master.pushRemote  [origin]

Configure repository defaults
 M-r pull.rebase        [true|false|default:false]
 M-p remote.pushDefault [origin]

Configure branch creation
 U branch.autoSetupMerge  [always|true|false|default:true]
 R branch.autoSetupRebase [always|local|remote|never|default:never]

