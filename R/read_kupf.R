#' @title Read Evaporation Experiment data from ku-pf Apparatur
#' @description Reads multiple ku-pf Apparatur files from a directory and returns them as list of data.tables.
#' @param path path to ku-pf files (character)
#' @param colnames.out colnames of output (default: c('ts', 'tens.up', 'tens.low', 'weight'))
#' @param firstrow.char character in first row of ku-pf files (default: 'Date/Time	Tension top	Tension bottom	Weight') (see details)
#' @param format.time_stamp POSIXct format of the time stamp column (column 1, here named 'ts') (see \code{\link{strptime}})
#' @param tz.time_stamp time zone of the time stamp column (column 1, here named 'ts') (default: 'GMT') (see \code{\link{as.POSIXct}})
#' @param ... arguments to \code{\link{list.files}}
#' @return list of the class dataSEM containing data.tables as list elements.
#' @details
#' \describe{\item{input file format:}{The standard file format of ku-pf files looks like: \cr
#' \cr
#' Date/Time	Tension top	Tension bottom	Weight \cr
#' kPa	kPa	g \cr
#' 01.11.2017 14:11:16	-0.48	-0.15	969.02 \cr
#' 01.11.2017 14:21:16	-0.47	-0.14	969.00 \cr
#' 01.11.2017 14:31:16	-0.46	-0.13	968.98 \cr
#' \cr
#' If the first row of the ku-pf files differs to the expression used here ('Date/Time	Tension top	Tension bottom	Weight"),
#' it can be set with firstrow.char. \cr}}
#' \describe{\item{sample_info:}{File names are added as attribut to the output (attr(out, 'sample_info')). \cr
#' \cr
#' The ku-pf software gives the possibility to add
#' sample spezific information in the first row of the file. Depending on the input the ku-pf files than look: \cr
#' \cr
#' sample_name;project \cr
#' Date/Time	Tension top	Tension bottom	Weight \cr
#' kPa	kPa	g \cr
#' 01.11.2017 14:11:16	-0.48	-0.15	969.02 \cr
#' 01.11.2017 14:21:16	-0.47	-0.14	969.00 \cr
#' 01.11.2017 14:31:16	-0.46	-0.13	968.98 \cr
#' \cr
#' - For this case the first line is added as attribute to the output (attr(out, 'sample_info')$info) \cr
#' }}
#' @author Ullrich Dettmann
#' @import data.table
#' @export
#'
# ToDo: add ignore.files
read.kupf <- function(path, colnames.out   = c('ts', 'tens.up', 'tens.low', 'weight'),
                      firstrow.char = 'Date/Time\tTension top\tTension bottom\tWeight',
                      format.time_stamp = '%d.%m.%Y %H:%M:%S', tz.time_stamp = 'GMT', ...) {


  if(is.null(list(...)$recursive)) {recursive <- FALSE}
  if(!is.null(list(...)$recursive)) {recursive <- list(...)$recursive}
  # list files
  kupf_files <- list.files(path = path, ...)

  if(recursive == FALSE) {
    # remove subdirectories of kupf_files if recursive is FALSE
    kupf_files <- kupf_files[!kupf_files %in% list.dirs(path, recursive = F, full.names = F)]
  }
  sample_info <- c()
  dt.kupf <- c()
  for (i in 1:length(kupf_files)) {
    dt <- fread(text = file.path(path, kupf_files[i]), fill = T, header = F, sep = '')
    start_row <- dt[V1 %in% firstrow.char, which = T]
    #
    sample_info[[i]] <- data.table(file_name = kupf_files[i])
    if(start_row == 2) { sample_info[[i]][, info := dt[1]]}

    dt.kupf[[i]] <- dt[start_row + 2L:.N, tstrsplit(V1, '\t')]
    setnames(dt.kupf[[i]], colnames.out)
    dt.kupf[[i]][, colnames.out[1] := as.POSIXct(get(colnames.out[1]), format = format.time_stamp, tz = tz.time_stamp)]
    dt.kupf[[i]][, colnames.out[2] := as.numeric(get(colnames.out[2]))]
    dt.kupf[[i]][, colnames.out[3] := as.numeric(get(colnames.out[3]))]
    dt.kupf[[i]][, colnames.out[4] := as.numeric(get(colnames.out[4]))]
    # remove rows which only contain NA value
    dt.kupf[[i]] <- dt.kupf[[i]][dt.kupf[[i]][, !Reduce(`&`, lapply(.SD, is.na))]]

    dt.kupf[[i]] <- dt.kupf[[i]][!is.na(get(colnames.out[1]))]

    dt.kupf[[i]][, file_name := kupf_files[i]]
    }
  sample_info <- rbindlist(sample_info, fill = T)
  setattr(dt.kupf, 'sample_info', sample_info)
  class(dt.kupf) <- 'dataSEM'
  dt.kupf
}
#
utils::globalVariables(c('V1', 'file_name', 'info')) #define global variables for data.table
