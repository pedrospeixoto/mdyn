
#!/bin/bash
echo ""
cd Workspace
rsync -avzu * pedrosp@ime.usp.br:www/covid19/iso_index/.
