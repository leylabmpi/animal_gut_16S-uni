set.seed(3846)

#' convert to numeric
as.Num = function(x){
    as.numeric(as.character(x))
}

#' simple dataframe summary
dfhead = function(df, n=3){
    df %>% dim %>% print
    df %>% head(n=n)
}

#' make directory
make_dir = function(dir){
    if(! dir.exists(dir)){
        dir.create(dir, showWarnings=FALSE)
	cat('Created directory:', dir, '\n')
    } else {
        cat('Directory already exists:', dir, '\n')
    }
}

#' bash job using conda env
bash_job = function(cmd, conda_env, stdout=TRUE, stderr=FALSE){
    # cmd : string; commandline job (eg., 'ls -thlc')
    # conda_env : string; conda environment name
    cmd = sprintf('source activate %s; %s', conda_env, cmd)
    cmd = sprintf('-c "%s"', cmd)
    system2('bash', cmd, stdout=stdout, stderr=stderr)
}

#' send and email
send_email = function(body, subject='R job complete', email='nyoungblut@tuebingen.mpg.de'){
    cmd = sprintf('echo %s | mail -s "%s" "%s"', body, subject, email)
    system(cmd)
}

#' conda list in R
condaInfo = function(conda_env){
    cat(paste(bash_job('conda list', conda_env), collapse='\n'))
}

#' pipeline sessionInfo
pipelineInfo = function(pipeline_path, head_n=10){
    # readme
    readme_path = file.path(pipeline_path, 'README.md')
    if(!file.exists(readme_path)){
        cat('Cannot find README.md file in pipeline directory')
	stop()
    }
    cmd = sprintf('head -n %s %s', head_n, readme_path)
    cat(paste(system(cmd, intern=TRUE), collapse='\n'))
    cat('\n\n--- conda envs ---\n')
    # conda envs
    env_path = file.path(pipeline_path, 'bin', 'envs')
    cmd = sprintf('find %s -name "*.yaml" | xargs head -n 1000', env_path)
    cat(paste(system(cmd, intern=TRUE), collapse='\n'))
}

#' create UUID for figure file name
fig_uuid = function(full=FALSE){
    baseuuid = paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
    
    if(full == TRUE){
       id = paste(
              substr(baseuuid,1,8), 
              "-",
              substr(baseuuid,9,12),
              "-",
              "4",
              substr(baseuuid,13,15),
              "-",
              sample(c("8","9","a","b"),1),
              substr(baseuuid,16,18),
              "-",
              substr(baseuuid,19,30),
              sep="",
              collapse="")
    } else{
        id = substr(baseuuid,1,24)
    }
    id = paste0('fig-', id)
    return(id)
}

#' plot figure and save the figure at the same time
plot_write = function(p, file=NULL, path=NULL, width=NA, height=NA, ...){
    # file path
    if(is.null(path)){
        path = file.path(getwd(), '.figures')
        if(! dir.exists(path)){
            dir.create(path, showWarnings=FALSE)
        }
    }
    # file name
    if(is.null(file)){
        file = paste0(fig_uuid(), '.pdf')
    } 
    file = file.path(path, file)
    
    # width & height
    if(is.na(width)){
        width = options()$repr.plot.width
    }
    if(is.na(height)){
        height = options()$repr.plot.height
    }
    # writting figure
    if(length(class(p)) >= 2 & class(p)[2] == 'ggplot2'){
        ggplot2::ggsave(filename=file, plot=p, width=width, height=height, ...)
    } else {
        pdf(file=file, width=width, height=height)
        plot(p, ...)
        dev.off()
    }
    cat('File written:', file, '\n')
    # plotting
    plot(p)
}
