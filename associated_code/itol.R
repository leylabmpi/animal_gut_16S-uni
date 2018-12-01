#' create itol symbol file
#' df = dataframe
#'   *) the rownames should correspond with the tree internal node labels
#'   *) other columns should be: symbol,size,color,fill,position,[label]
itol_symbol = function(df, dataset_label, out_file, out_dir=NULL, legend=NULL, MAXIMUM_SIZE=50){
    if(! is.null(out_dir)){
        out_file = file.path(out_dir, out_file)
    }
    # Main params
    cat('DATASET_SYMBOL\n', file=out_file)
    cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
    cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
    cat('COLOR #ff0000\n', file=out_file, append=TRUE)
    cat(sprintf('MAXIMUM_SIZE %s\n', MAXIMUM_SIZE), file=out_file, append=TRUE)

    # # legend
    # cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
    # if(is.null(legend)){
    #     shapes = rep(1, colnames(df) %>% length)
    #     cols = gsub('FF$', '', rainbow(length(shapes)))
    #     labs = paste(gsub(' ', '_', colnames(df)), collapse=' ')
    # } else {
    #     stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
    #     shapes = legend$shapes %>% as.character
    #     cols = legend$colors %>% as.character
    #     labs = legend$labels %>% as.character
    # }
    # cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')), 
    #     file=out_file, append=TRUE)
    # cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')), 
    #     file=out_file, append=TRUE)
    # cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

    # Data
    cat('DATA\n', file=out_file, append=TRUE)
    write.table(df, file=out_file, append=TRUE, sep=' ', 
                quote=FALSE, row.names=TRUE, col.names=FALSE)
    cat('File written:', out_file, '\n')
}


#' create itol multi-bar file
#' df = dataframe; the rownames should correspond with the tree labels
itol_multibar = function(df, dataset_label, out_file, out_dir=NULL, legend=NULL, WIDTH=200){
    if(! is.null(out_dir)){
        out_file = file.path(out_dir, out_file)
    }
    # Main params
    cat('DATASET_MULTIBAR\n', file=out_file)
    cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
    cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
    cat('COLOR #ff0000\n', file=out_file, append=TRUE)
    cat(sprintf('WIDTH %s\n', WIDTH), file=out_file, append=TRUE)

    # Field labels
    labs = gsub(' ', '_', colnames(df))
    labs = sprintf('FIELD_LABELS %s\n', paste(labs, collapse=' '))
    cat(labs, file=out_file, append=TRUE)

    # Field colors
    if(is.null(legend)){
        cols = gsub('FF$', '', rainbow(ncol(df)))
    } else {
        cols = legend$colors %>% as.character
    }
    cols = sprintf('FIELD_COLORS %s\n', paste(cols, collapse=' '))
    cat(cols, file=out_file, append=TRUE)

    # legend
    cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
    if(is.null(legend)){
        shapes = rep(1, colnames(df) %>% length)
        cols = gsub('FF$', '', rainbow(length(shapes)))
        labs = paste(gsub(' ', '_', colnames(df)), collapse=' ')
    } else {
        stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
        shapes = legend$shapes %>% as.character
        cols = legend$colors %>% as.character
        labs = legend$labels %>% as.character
    }
    cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')), 
        file=out_file, append=TRUE)
    cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')), 
        file=out_file, append=TRUE)
    cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

    # Data
    cat('DATA\n', file=out_file, append=TRUE)
    write.table(df, file=out_file, append=TRUE, sep=' ', 
                quote=FALSE, row.names=TRUE, col.names=FALSE)
    cat('File written:', out_file, '\n')
}


#' create itol boxplot file
#' df = dataframe; the rownames should correspond with the tree labels; the columns must specify: minimum,q1,median,q3,maximum,extreme_value1,extreme_value2
itol_boxplot = function(df, dataset_label, out_file, out_dir=NULL){
    if(! is.null(out_dir)){
        out_file = file.path(out_dir, out_file)
    }
    # Main params
    cat('DATASET_BOXPLOT\n', file=out_file)
    cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
    cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
    cat('COLOR #ff0000\n', file=out_file, append=TRUE)

    # data
    cat('DATA\n', file=out_file, append=TRUE)
    write.table(df, file=out_file, append=TRUE, sep=' ', 
                quote=FALSE, row.names=TRUE, col.names=FALSE)
    cat('File written:', out_file, '\n')
}

#' create itol heatmap file
#' df = dataframe; the rownames should correspond with the tree labels; all columns should be numeric values for the heatmap
#' dataset_label = label for the dataset
#' tree = tree used for ordering the heatmap columns; if NULL, the dist_method will be used to create the tree
#' color_scheme = heatmap color scheme. color = blue-orange-yellow; bw=white-grey-black
itol_heatmap = function(df, dataset_label, out_file, out_dir=NULL, tree=NULL,
                        dist_method='bray', color_scheme=c('color', 'bw')){		
    if(! is.null(out_dir)){
        out_file = file.path(out_dir, out_file)
    }
    
    # Main params
    cat('DATASET_HEATMAP\n', file=out_file)
    cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
    cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
    cat('COLOR #ff0000\n', file=out_file, append=TRUE)

    # Field labels
    labs = gsub(' ', '_', colnames(df))
    labs = sprintf('FIELD_LABELS %s\n', paste(labs, collapse=' '))
    cat(labs, file=out_file, append=TRUE)
    
    ## colors
    cat('COLOR_NAN #eae8e8\n', file=out_file, append=TRUE)
    cat('USE_MID_COLOR 1\n', file=out_file, append=TRUE) 
    if(color_scheme == 'color'){
    	cat('COLOR_MIN #0000ff\n', file=out_file, append=TRUE)  
	cat('COLOR_MAX #ffff00\n', file=out_file, append=TRUE)  
        cat('COLOR_MID #ff8000\n', file=out_file, append=TRUE)
    } else
    if(color_scheme == 'bw'){
    	cat('COLOR_MIN #ffffff\n', file=out_file, append=TRUE)  
	cat('COLOR_MAX #cccccc\n', file=out_file, append=TRUE)  
        cat('COLOR_MID #000000\n', file=out_file, append=TRUE)
    } else {
        stop('color_scheme not recoginized')
    }

    # creating correlation tree
    if(is.null(tree) & !is.null(dist_method)){
        tree = df %>% t %>% vegan::vegdist(method=dist_method) %>% hclust %>% ape::as.phylo()    
    }
    if(!is.null(tree)){
        cat('FIELD_TREE ', file=out_file, append=TRUE)
        write.tree(tree, file=out_file, append=TRUE)
    }

    # data
    cat('DATA\n', file=out_file, append=TRUE)
    write.table(df, file=out_file, append=TRUE, sep=' ', 
                quote=FALSE, col.names=FALSE)
    cat('File written:', out_file, '\n')
}

#' create itol colorstrip
#' df = dataframe; the rownames should correspond with the tree labels; the plotting parameter should be column 1
#' dataset_label = label for the dataset
#' legend = specify particular legend
itol_colorstrip = function(df, dataset_label, out_file, out_dir=NULL, legend=NULL){
    df = data.frame(tip = rownames(df),
                    feature = as.character(df[,1]))
    if(! is.null(out_dir)){
        out_file = file.path(out_dir, out_file)
    }    
    # main options
    cat('DATASET_COLORSTRIP\n', file=out_file)
    cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
    cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
    cat('COLOR #ff0000\n', file=out_file, append=TRUE) 

    # legend
    cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
    if(is.null(legend)){
        shapes = rep(1, df[,2] %>% unique %>% length)
        cols = gsub('FF$', '', rainbow(length(shapes)))
        labs = paste(gsub(' ', '_', df[,2] %>% unique), collapse=' ')
    } else {
        stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
        shapes = legend$shapes %>% as.character
        cols = legend$colors %>% as.character
        labs = legend$labels %>% as.character
    }
    cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')), 
        file=out_file, append=TRUE)
    cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')), 
        file=out_file, append=TRUE)
    cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

    # data
    ## adding colors
    if(is.null(legend)){
      cols = data.frame(feature = df[,2] %>% unique,
                        color = cols)
    } else {
      cols = legend[,c('labels', 'colors')]
      colnames(cols) = c('feature', 'color')
    }
    df = df %>%
        inner_join(cols, c('feature')) %>% 
        dplyr::select(tip, color, feature) %>%
        as.data.frame
    ## writing
    cat('DATA\n', file=out_file, append=TRUE)
    write.table(df, file=out_file, append=TRUE, sep=' ', 
                quote=FALSE, col.names=FALSE, row.names=FALSE)
    cat('File written:', out_file, '\n')
}
