read_ris <- function(file_path,
                     sep = "; ",
                     encoding = "UTF-8",
                     skipNul = TRUE) {
    con <- file_path
    raw_lines <- readLines(con = con,
                           encoding = encoding,
                           skipNul = skipNul)
    parsed_data <- list()  # 存储所有参考文献
    current_entry <- list()  # 存储单个参考文献
    line_pattern <- "^[A-Z][A-Z0-9]  - "  # 标签行的正则表达式
    
    for (line in raw_lines) {
        # 检查是否为标签行
        if (grepl(pattern = line_pattern, x = line)) {
            tag <- substr(x = line,
                          start = 1,
                          stop = 2) # 提取标签
            # 遇到 ER 表示一个参考文献的结束
            if (tag == "ER") {
                parsed_data <- append(parsed_data, list(current_entry))  # 保存当前参考文献
                current_entry <- list()  # 清空，用于下一条文献
                next  # 跳过 ER 标签
            }
            
            content <- substr(x = line,
                              start = 7,
                              stop = nchar(line)) # 提取内容
            
            # 合并同一条目相同标签的内容
            if (tag %in% names(current_entry)) {
                current_entry[[tag]] <- paste(current_entry[[tag]], content, sep = sep)  
            } else {
                current_entry[[tag]] <- content
            }
        }
    }
    
    # 找出所有出现过的标签
    all_tags <- unique(unlist(lapply(parsed_data, names)))
    # 将每个文献填充为拥有所有标签的列表，并转为数据框
    ris_df <- do.call(rbind, lapply(parsed_data, function(entry) {
        sapply(all_tags, function(tag)
            if (!is.null(entry[[tag]]))
                entry[[tag]]
            else
                NA)  # 填充缺失标签为 NA
    }))
    
    
    ris_df <- as.data.frame(ris_df, stringsAsFactors = FALSE)
    
    ris_df
}