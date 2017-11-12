sh ./build.sh
aws s3 cp public/ s3://regnsjekk-website --recursive --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
