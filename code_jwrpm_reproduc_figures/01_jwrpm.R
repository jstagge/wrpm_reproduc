# *------------------------------------------------------------------
# | PROGRAM NAME:
# | FILE NAME: .R
# | DATE:
# | CREATED BY:  Jim Stagge
# *----------------------------------------------------------------
# | PURPOSE:
# |
# |
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
require(here)

### Path for Data and Output
data_path <- file.path(here(), "data")
#output_path <- "/fs/ess/PAS1921"
output_path <- file.path(here(), "output")

### Set up output folders
write_output_path <- output_path
# write_output_path <- file.path(output_path, "ar")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Set up figure folder
write_figures_path <- file.path(output_path, "figures")
dir.create(write_figures_path, recursive=TRUE, showWarnings = FALSE)

###########################################################################
###  Load functions
###########################################################################
require(tidyverse)
require(tictoc)
require(viridis)

library(readxl)
require(ggrepel)
### For Dates
require(lubridate)

select <- dplyr::select

###########################################################################
###  Read in data
###########################################################################
read_file <- file.path(data_path, 'JWRPM OA vs non-OA Papers Stats 6-28-2023.xlsx')
excel_sheets(read_file)
oa_df <- read_excel(read_file, sheet = "OA Country (2021-Now)")
non_oa_df <- read_excel(read_file, sheet =  "Non-OA Country (2021-Now)")

gdp_df <- read_excel(file.path(data_path, 'gdp_worldometer.xlsx'))


###########################################################################
###  Clean up  data and join
###########################################################################
oa_df <- oa_df %>%
	rename(oa = 1, country = 2, oa_abstract_views = 3, oa_downloads = 4)

non_oa_df <- non_oa_df %>%
	rename(oa = 1, country = 2, nonoa_abstract_views = 3, nonoa_downloads = 4)

combined_df <- oa_df %>%
	select(-oa) %>%
	full_join(non_oa_df %>%	select(-oa), by = "country")

combined_df <- combined_df %>% left_join(gdp_df, by = "country")

###########################################################################
###  Quick plot
###########################################################################
p <- ggplot(combined_df, aes(x=nonoa_abstract_views , y = oa_abstract_views, label = country)) %>%
	+ geom_point(colour = "grey40", size = 1.6) %>%
#	+ geom_text_repel(max.overlaps = 9, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Abstract Views from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Abstract Views from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
#	+ scale_size(name = "GDP") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p

#accuracy = 0
### Save plot
plot_name <- paste0("abstract_views_oa_va_nonoa_clean.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 5, height = 5, dpi = 600)






p <- ggplot(combined_df, aes(x= nonoa_downloads, y =  oa_downloads, label = country)) %>%
	+ geom_point(colour = "grey40", size = 1.6) %>%
#	+ geom_text_repel(max.overlaps = 9, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Downloads from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p

#accuracy = 0
### Save plot
plot_name <- paste0("downloads_oa_va_nonoa_clean.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 5, height = 5, dpi = 600)


p <- ggplot(combined_df, aes(x= nonoa_downloads, y =  oa_downloads, label = country)) %>%
	+ geom_point(colour = "grey40", size = 1.4) %>%
	+ geom_text_repel(max.overlaps = 9, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Downloads from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p

#accuracy = 0
### Save plot
plot_name <- paste0("downloads_oa_va_nonoa_bycountry.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 5, height = 5, dpi = 600)


p <- ggplot(combined_df, aes(x= nonoa_downloads, y =  oa_downloads, label = country)) %>%
	+ geom_point(colour = "grey40", aes(size = gdp_2017)) %>%
	+ geom_text_repel(max.overlaps = 7, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Downloads from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p

#accuracy = 0
### Save plot
plot_name <- paste0("downloads_oa_va_nonoa_bycountry_gdp.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)



p <- ggplot(combined_df, aes(x= nonoa_downloads, y =  oa_downloads, label = country)) %>%
	+ geom_point( size = 3, aes(colour = log10(gdp_2017))) %>%
	#+ geom_text_repel(max.overlaps = 7, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Downloads from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP") %>%
	+ scale_colour_viridis(name = "GDP\n($ 10^)") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p
### Save plot
plot_name <- paste0("downloads_oa_va_nonoa_bycountry_gdp_colour.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)


p <- ggplot(combined_df, aes(x= nonoa_downloads, y =  oa_downloads, label = country)) %>%
	+ geom_point( size = 3, aes(colour = log10(population_2017))) %>%
	#+ geom_text_repel(max.overlaps = 7, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Downloads from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP") %>%
	+ scale_colour_viridis(name = "Population\n( 10^)") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p
### Save plot
plot_name <- paste0("downloads_oa_va_nonoa_bycountry_population_colour.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)


p <- ggplot(combined_df, aes(x= nonoa_downloads, y =  oa_downloads, label = country)) %>%
	+ geom_point( size = 3, aes(colour = log10(gdp_percapita))) %>%
	#+ geom_text_repel(max.overlaps = 7, min.segment.length = 0, nudge_x = 0.15, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Downloads from non-OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads from OA", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP") %>%
	+ scale_colour_viridis(name = "GDP per capita\n( 10^)") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p
### Save plot
plot_name <- paste0("downloads_oa_va_nonoa_bycountry_gdppercapita_colour.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)


combined_df <- combined_df %>%
	mutate(download_ratio = oa_downloads/nonoa_downloads)

p <- ggplot(combined_df, aes(x= gdp_percapita, y =  download_ratio, label = country)) %>%
		+ geom_point( size = 1.5) %>%
		+ geom_abline(slope =0, intercept = 0, linetype = "longdash") %>%
		+ geom_text_repel(max.overlaps = 5, min.segment.length = 0, nudge_x = 0.05, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%

		+ scale_x_log10(name = "GDP per capita ($)", breaks = c(1e-3, 1e-2, 1e-1,1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7)) %>%
	#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
		+ scale_y_log10(name = "Ratio OA/non-OA Downloads", breaks =  c(1e-3, 1e-2, 1e-1,1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7)) %>%

		+ theme_classic() %>%
		+ annotation_logticks( sides = "lb",
			short = unit(.5,"mm"),
			mid = unit(3,"mm"),
			long = unit(4,"mm")
		)

		### Save plot
		plot_name <- paste0("download_ratio.png")
		ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)



plot_df <- oa_df %>%
	rename(abstract_views = 3, downloads = 4) %>%
	bind_rows(non_oa_df %>% rename(abstract_views = 3, downloads = 4) ) %>%
	#full_join(non_oa_df %>%	select(-oa), by = "country") %>%
	left_join(gdp_df, by = "country")


p <- ggplot(plot_df, aes(x=abstract_views, y = downloads, colour = oa, label = country)) %>%
	+ geom_point() %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ scale_x_log10(name = "Abstract Views", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
#	+ scale_size(name = "GDP") %>%
	+ scale_colour_brewer(name = "Open Access \n Manuscript", type = "qual", palette = "Set1") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p

### Save plot
plot_name <- paste0("download_vs_abstractviews_oa_vs_non_clean.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)




p <- ggplot(plot_df, aes(x=abstract_views, y = downloads, colour = oa, label = country)) %>%
	+ geom_point() %>%
	+ geom_abline(slope =1, intercept = 0, linetype = "longdash") %>%
	+ geom_text_repel(max.overlaps = 5, min.segment.length = 0, nudge_x = 0.05, nudge_y = 0.1, segment.colour="red", size = 2.3) %>%
	+ scale_x_log10(name = "Abstract Views", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = label_comma(accuracy=1)) %>%
#    breaks = c(1, 10, 1e2, 1e3), label = ~ scales::comma(.x, accuracy = 1)  )	%>%
	+ scale_y_log10(name = "Downloads", breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels =  label_comma(accuracy = 1)) %>%
	+ scale_size(name = "GDP per capita") %>%
	+ scale_colour_brewer(name = "Open Access \n Manuscript", type = "qual", palette = "Set1") %>%
	+ annotation_logticks( sides = "lb",
	  short = unit(.5,"mm"),
	  mid = unit(3,"mm"),
	  long = unit(4,"mm")
	) %>%
	+ coord_fixed(ratio = 1) %>%
	+ theme_classic(9) %>%
	+ theme(panel.grid.minor = element_blank())

p

### Save plot
plot_name <- paste0("download_vs_abstractviews_oa_vs_non_label.png")
ggsave(file.path(write_figures_path, plot_name), p,  width = 6, height = 5, dpi = 600)
