library(reticulate)
library(textTinyR)
library(fastTextR)

clust_vec = textTinyR::tokenize_transform_vec_docs(object = data$Text, as_token = T,
                                                   to_lower = T, 
                                                   remove_punctuation_vector = F,
                                                   remove_numbers = F, 
                                                   trim_token = T,
                                                   split_string = T,
                                                   split_separator = " \r\n\t.,;:()?!//", 
                                                   remove_stopwords = T,
                                                   language = "english", 
                                                   min_num_char = 3, 
                                                   max_num_char = 100,
                                                   stemmer = "porter2_stemmer", 
                                                   threads = 4,
                                                   verbose = T)

unq = unique(unlist(clust_vec$token, recursive = F))
length(unq)


# I'll build also the term matrix as I'll need the global-term-weights

utl = textTinyR::sparse_term_matrix$new(vector_data = data$Text, file_data = NULL,
                                        document_term_matrix = TRUE)

tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                     remove_numbers = F, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)

gl_term_w = utl$global_term_weights()

str(gl_term_w)

save_dat = textTinyR::tokenize_transform_vec_docs(object = data$Text, as_token = T, 
                                                  to_lower = T, 
                                                  remove_punctuation_vector = F,
                                                  remove_numbers = F, trim_token = T, 
                                                  split_string = T, 
                                                  split_separator = " \r\n\t.,;:()?!//",
                                                  remove_stopwords = T, language = "english", 
                                                  min_num_char = 3, max_num_char = 100, 
                                                  stemmer = "porter2_stemmer", 
                                                  path_2folder = "/Users/pedram/Desktop/Trump-Tweets.nosync/data/",
                                                  threads = 1,                     # whenever I save data to file set the number threads to 1
                                                  verbose = T)
PATH_INPUT = "/Users/pedram/Desktop/Trump-Tweets.nosync/data/output_token_single_file.txt"

PATH_OUT = "/Users/pedram/Desktop/Trump-Tweets.nosync/data//rt_fst_model"


vecs = fastTextR::skipgram_cbow(input_path = PATH_INPUT, output_path = PATH_OUT, 
                                method = "skipgram", lr = 0.075, lrUpdateRate = 100, 
                                dim = 300, ws = 5, epoch = 5, minCount = 1, neg = 5, 
                                wordNgrams = 2, loss = "ns", bucket = 2e+06,
                                minn = 0, maxn = 0, thread = 6, t = 1e-04, verbose = 2)

init = textTinyR::Doc2Vec$new(token_list = clust_vec$token, 
                              
                              word_vector_FILE = "/Users/pedram/Desktop/Trump-Tweets.nosync/data/rt_fst_model.vec",
                              
                              print_every_rows = 5000, 
                              
                              verbose = TRUE, 
                              
                              copy_data = FALSE)                  # use of external pointer




setwd('/Users/pedram/Desktop/fastTextR')
Rcpp::compileAttributes(verbose = TRUE)
setwd('/Users/pedram/Desktop')
system("R CMD build fastTextR")
system("R CMD INSTALL fastTextR_1.0.2.tar.gz")
