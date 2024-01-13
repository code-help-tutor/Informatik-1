WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com

# shell commands for Informatik 2 to run C0 programs in docker
docker_c0() { docker run --rm --user=$(id -u):$(id -g) -v "$(pwd)":/code -w /code -it dbatunituebingen/c0:latest $@; }
execc0() { docker_c0 ./$@; }
cc0() { docker_c0 cc0 $@; }
coin() { docker_c0 coin $@; }
codex() { docker_c0 codex $@; }