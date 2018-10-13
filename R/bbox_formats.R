library(tidyverse)

bbox_vector = function(bbox = "", format = "femh") {
  # return list of c(x1, y1, x2, y2)
  library(stringr)

  if (is.na(bbox)) { return(c(NA,NA,NA,NA)) }

  if (format == "femh") {
    bboxes = str_match_all(bbox, "\\(.*?\\)")[[1]]
    r = list()
    for (i in seq_along(bboxes)) {
	p = str_match_all(bboxes[i], "\\d+")[[1]]
	if (length(p) == 0 | length(p) %% 4) {
	    cat("Format error!\n")
	    return(NA)
	}
	r[[i]] = c(p[1], p[2], p[3], p[4])
	names(r[[i]]) = c("xmin","ymin","xmax","ymax")
    }
    return(r)
  }
}

parse_bbox = function(bbox_str = "", color = "red", strokewidth = 2) {
  library(stringr)

  if (is.na(bbox_str)) { return("") }

  bboxes = str_match_all(bbox_str, "\\(.*?\\)")
  r = paste("-fill none -stroke", color)
  for (bbox in bboxes[[1]]) {
    p = str_match_all(bbox, "\\d+")[[1]]
    r = c(r, paste(
      "-strokewidth", strokewidth,
      '-draw "rectangle',
      paste(p[1],p[2],sep=","),
      paste(p[3],p[4],sep=","),
      '"'))
  }
  r %>% paste(collapse = " ") %>% return
}

bbox_fmt = function(bbox = "", from = "femh", to = "rsna") {
  library(stringr)
  library(purrr)

  if (is.na(bbox)) { return("") }

  if (from == "femh" & to == "rsna") {
      bboxes = str_match_all(bbox, "\\(.*?\\)")[[1]]
      r = character(0)
      for (b in bboxes) {
	p = str_match_all(b, "\\d+")[[1]] %>% as.integer()

	r = c(r, paste("0.5", p[1], p[2], p[3]-p[1], p[4]-p[2]))
      }
      r %>% paste(collapse = " ") %>% return
  }

  if (from == "rsna" & to == "femh") {
      p = str_match_all(b, "(\\d\\.)+")[[1]] %>% as.numeric()
      if (length(p) == 0 | length(p) %% 5) return("Error! number not multiples of 5")
      n = length(p) / 5
      r = character(0)
      for (i in 0:(n-1)) {
	r = c(r, paste0("(", p[5*n+2], ".", p[5*n+3], ".", p[5*n+4], ".", p[5*n+5], ")"))
      }
      r %>% paste(collapse = " ") %>% return
  }

  return("Error!")
}

bbox2xml = function(csv = NA, xmldir = "xml", id_col = 1, bbox_col = 3) {
    library(xml2)
    library(tidyverse)

    if (!dir.exists(xmldir)) dir.create(xmldir)
    for (i in seq_along(csv)) 
    {
	if (!file.exists(i)) {
	    cat(i, "not exists!\n")
	    break
	}
	x = csv[i] %>% read_csv() %>% drop_na()
	for (j in 1:nrow(x))
	{
	    filename = as.character(x[j, id_col]) %>% paste0(".png")
	    cat(j, filename, "\n")
	    ann = xml_new_document() %>% xml_add_child("annotation")
	    ann %>% xml_add_child("folder") %>% xml_set_text("class")
	    ann %>% xml_add_child("filename") %>% xml_set_text(filename)
	    ann %>% xml_add_child("path") %>% xml_set_text(filename)
	    ann %>% xml_add_child("source") %>% xml_add_child("database") %>% xml_set_text("Unknown")
	    ann %>% xml_add_child("size") %>% 
		xml_add_child("width") %>% xml_set_text("1024") %>%
		xml_add_sibling("height") %>% xml_set_text("1024") %>%
		xml_add_sibling("depth") %>% xml_set_text("1") 

	    bboxs = x[j, bbox_col] %>% bbox_vector # str_extract_all("\\(.*?\\)") %>% .[[1]]
	    for (bbox in bboxs) 
	    {
		#xy = bbox %>% str_extract_all("\\d+") %>% .[[1]]
		ann %>% xml_add_child("object") %>% 
		    xml_add_child("name") %>% xml_set_text("class") %>%
		    xml_add_sibling("pose") %>% xml_set_text("Unspecified") %>%
		    xml_add_sibling("truncated") %>% xml_set_text("0") %>%
		    xml_add_sibling("difficult") %>% xml_set_text("0") %>%
		    xml_add_sibling("bndbox") %>% 
			xml_add_child("xmin") %>% xml_set_text(bbox["xmin"]) %>%
			xml_add_child("ymin") %>% xml_set_text(bbox["ymin"]) %>%
			xml_add_child("xmax") %>% xml_set_text(bbox["xmax"]) %>%
			xml_add_child("ymax") %>% xml_set_text(bbox["ymax"]) 
			#xml_add_child("xmin") %>% xml_set_text(xy[1]) %>%
			#xml_add_child("ymin") %>% xml_set_text(xy[2]) %>%
			#xml_add_child("xmax") %>% xml_set_text(xy[3]) %>%
			#xml_add_child("ymax") %>% xml_set_text(xy[4]) 
	    }

	    xmlfile = as.character(x[j, id_col]) %>% paste0(".xml")
	    write_xml(ann, file.path(xmldir, xmlfile))
	}
    }
}

