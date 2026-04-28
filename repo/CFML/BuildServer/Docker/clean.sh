# Return the docker containers that are exited
exited_containers=`docker ps -q -f "status=exited"`
# If some have been found, remove them
if [ -n "$exited_containers" ]
then
    docker rm ${exited_containers}
fi

# Return the docker images that are in dangling state
images=`docker images -q -f "dangling=true"`
# If some dangling images have been found, remove them
if [ -n "$images" ]
then
    docker rmi ${images}
fi

# Also remove docker volumes
volumes=`docker volume ls -q -f "dangling=true"`
# If some dangling volumes have been found, remove them
if [ -n "$volumes" ]
then
    docker volume rm ${volumes}
fi