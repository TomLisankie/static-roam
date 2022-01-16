# TODO needs work

source bin/setup.sh
bin/fetch.sh
bin/publish.sh

bin/git-commit.sh

# Check the results (note: this usually gets an old cached copy, not sure how to fix that)
open http://www.hyperphor.com/ammdi/pages/New.html
