hello_world <- function(){
  return("hello world")
}

my_sum <- function(a, b){
  return(sum(a,b))
}

my_hector_run <- function(){

  ini <- system.file(package = "hector", "input/hector_ssp119.ini")
  core <- newcore(ini)
  run(core)
  out <- fetchvars(core, dates = 1850:2020)
  return(out)


}
