# So Idea is to 
# 1. save/print the multipage original received from Nicola into single spreads name them as cover_region.pdf
# 2. then to make the barcodes into same dimension and so that the barcode is placed into the white space
# 3. run the codes below



# RAP
pdfcrop --margins '-331 -0 -0 -0' RAP_cover.pdf front_cover_RAP.pdf
pdfcrop --margins '-0 -0 -331 -0' RAP_cover.pdf back_cover_RAP.pdf

pdftk back_cover_RAP.pdf stamp 'RAP I5102E barcode1.pdf' output back_cover_RAP_bc.pdf

# RAF
pdfcrop --margins '-331 -0 -0 -0' RAF_cover.pdf front_cover_RAF.pdf
pdfcrop --margins '-0 -0 -331 -0' RAF_cover.pdf back_cover_RAF.pdf

pdftk back_cover_RAF.pdf stamp 'RAF I5103E barcode1.pdf' output back_cover_RAF_bc.pdf

# REU
pdfcrop --margins '-331 -0 -0 -0' REU_cover.pdf front_cover_REU.pdf
pdfcrop --margins '-0 -0 -331 -0' REU_cover.pdf back_cover_REU.pdf

pdftk back_cover_REU.pdf stamp 'REU I5104E barcode1.pdf' output back_cover_REU_bc.pdf

# RNE
pdfcrop --margins '-331 -0 -0 -0' RNE_cover.pdf front_cover_RNE.pdf
pdfcrop --margins '-0 -0 -331 -0' RNE_cover.pdf back_cover_RNE.pdf

pdftk back_cover_RNE.pdf stamp 'RNE I5105E barcode1.pdf' output back_cover_RNE_bc.pdf


################


# COF
# New small one
pdfcrop --margins '-332 -0 -0 -0' COVER_FaoPocket_2015_Cofee-rev1-light.pdf front_cover_COF.pdf
pdfcrop --margins '-0 -0 -332 -0' COVER_FaoPocket_2015_Cofee-rev1-light.pdf back_cover_COF.pdf







# File with crop marks (HUGE 52MB)
pdfcrop --margins '-363 -33 -33 -33' COVER_FaoPocket_2015_Cofee-rev2.pdf front_cover_COF.pdf
pdfcrop --margins '-33 -33 -363 -33' COVER_FaoPocket_2015_Cofee-rev2.pdf back_cover_COF.pdf
