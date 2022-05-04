dirs: data.derived figures fonts
	@mkdir data.derived figures fonts
	
fonts: fonts/NotoSerif-unhinted.zip
	@mkdir fonts
	(cd fonts && \
		wget https://noto-website-2.storage.googleapis.com/pkgs/NotoSerif-unhinted.zip && \
		unzip NotoSerif-unhinted.zip && \
		rm NotoSerif-unhinted.zip)
		
data.raw: immune_cell_types tcc_expression
	#rmarkdown::render_stuff
	
immune_cell_types: data.derived/config_immune_cell_types.rds
data.derived/config_immune_cell_types.rds: data.raw/Immune_Cell_Types.csv data.raw/import_cell_types.R
	Rscript -e "source('data.raw/import_cell_types.R');import_cell_types()"
	
tcc_expression: data.derived/tcc_primary_exprset.rds
data.derived/tcc_primary_exprset.rds: data.raw/tcc_v2_20327_debatched_sample_col_probeset_row.txt data.raw/tcc_10469_barcodes_vetted.txt data.raw/tcc_subcategories_merged_barcodes.txt
	Rscript -e "source('data.raw/import_gene_expression.R');import_TCC_expression()"