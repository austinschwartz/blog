rm -rf _site
./site clean
./site build
rsync -avz _site/ root@austinschwartz.com:/var/www/austinschwartz.com/
