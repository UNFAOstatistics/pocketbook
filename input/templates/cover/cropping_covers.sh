# New small one
pdfcrop --margins '-332 -0 -0 -0' COVER_FaoPocket_2015_Cofee-rev1-light.pdf front_cover_COF.pdf
pdfcrop --margins '-0 -0 -332 -0' COVER_FaoPocket_2015_Cofee-rev1-light.pdf back_cover_COF.pdf


# File with crop marks (HUGE 52MB)
pdfcrop --margins '-363 -33 -33 -33' COVER_FaoPocket_2015_Cofee-rev2.pdf front_cover_COF.pdf
pdfcrop --margins '-33 -33 -363 -33' COVER_FaoPocket_2015_Cofee-rev2.pdf back_cover_COF.pdf