
inputs_df <- test$inputs_df


library(tidyr)
input_refs <- unnest(inputs_df, data.references)
input_refs <- data.frame(input_lensID = input_refs$data.lens_id, reference_lensID = input_refs$lens_id, type = 'reference')

input_cits <- unnest(inputs_df, data.scholarly_citations)
input_cits <- data.frame(input_lensID = input_cits$data.lens_id, reference_lensID = input_cits$data.scholarly_citations, type = 'citation')

network <- rbind(input_refs, input_cits)
write.csv(network, 'inst/extdata/network.csv')
