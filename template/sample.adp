gnatmake_opt=-g -i
main=./harness
main_unit=harness
build_dir=./
check_cmd=${cross_prefix}gcc -x ada -c ${comp_opt} ${full_current} -gnats
make_cmd=cd ${build_dir}
make_cmd=${cross_prefix}gnatmake -o ${main} ${main_unit} ${gnatmake_opt} -cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}
comp_cmd=cd ${build_dir}
comp_cmd=${cross_prefix}gcc -x ada -c ${comp_opt} ${full_current}
run_cmd=cd ${build_dir} && ${main} 
src_dir=./
src_dir=../aunit/
src_dir=../aunit/framework/
src_dir=../aunit/text_reporter/
obj_dir=./
obj_dir=../aunit/
obj_dir=../aunit/framework/
obj_dir=../aunit/text_reporter/


