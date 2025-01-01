read_bib <- function(file_path,
                     encoding = "UTF-8",
                     skipNul = TRUE) {
    con <- file_path
    raw_lines <- readLines(con = con,
                           encoding = encoding,
                           skipNul = skipNul)
    
    entries <- list()
    current_entry <- list()
    in_entry <- FALSE
    
    type_pattern <- "^@(\\w+)\\{([a-zA-Z0-9]*),?"
    field_pattern <- "\\s*(\\w+)\\s*=\\s*\\{(.*)\\},?"
    
    for (line in raw_lines) {
        # 跳过注释行，空行和结束行
        if (grepl("^%", line) || line %in% c("", "}") || grepl("^(Record)",line))
            next
        
        # 文献开始
        if (grepl(pattern = type_pattern, x =  line)) {
            if (in_entry) {
                # 保存之前的文献
                entries <- append(entries, list(current_entry))
            }
            in_entry <- TRUE
            current_entry <- list()  # 初始化新条目
            current_entry$type <- gsub(type_pattern, "\\1", line)  # 提取文献类型
            current_entry$key <- gsub(type_pattern, "\\2", line)  # 提取文献标识符
        }
        else if (in_entry) {
            # 解析文献字段
            field <- gsub(pattern = field_pattern, "\\1", line)
            value <- gsub(pattern = field_pattern, "\\2", line)
            current_entry[[field]] <- value
        }
    }
    
    # 保存最后一个文献
    if (in_entry) {
        entries <- append(entries, list(current_entry))
    }
    
    # 找出所有出现过的标签
    all_tags <- unique(unlist(lapply(entries, names)))
    # 将每个文献填充为拥有所有标签的列表，并转为数据框
    bib_df <- do.call(rbind, lapply(entries, function(entry) {
        sapply(all_tags, function(tag)
            if (!is.null(entry[[tag]]))
                entry[[tag]]
            else
                NA)  # 填充缺失标签为 NA
    }))
    
    bib_df <- as.data.frame(bib_df, stringsAsFactors = FALSE)
    
    return(bib_df)
}