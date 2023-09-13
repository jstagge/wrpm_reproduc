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
read_file <- file.path(data_path, 'WR Reproducibility New Report.xlsx')
excel_sheets(read_file)

under_review_df <- read_excel(read_file, sheet = "Papers Under Review")
accepted_silver_df <- read_excel(read_file, sheet = "Accepted AER Papers Silver")
accepted_bronze_df <- read_excel(read_file, sheet = "Accepted AER Papers Bronze")
declined_df <- read_excel(read_file, sheet = "Declined")
accepted_noreproduc_df <- read_excel(read_file, sheet = "Accepted no Reproducibility")

###########################################################################
###  Clean up  data and join
###########################################################################
accepted_silver_df <- accepted_silver_df %>%
	rename(ms_num = 1) %>%
	rename(author = 2) %>%
	rename(date = 3) %>%
	rename(doi = 4) %>%
	rename(date_online = 5) %>%
	rename(badge = 6) %>%
	rename(fee_waive = 7) %>%
	drop_na(ms_num) %>%
	rename(ae_decisions = badge) %>%
	mutate(art_type = "published")  %>%
	mutate(month = month(date), year = year(date)) %>%
	select(ms_num, date, month, year, art_type, ae_decisions) %>%
	arrange(date)

accepted_bronze_df <- accepted_bronze_df %>%
		rename(ms_num = 1) %>%
		rename(author = 2) %>%
		rename(date = 3) %>%
		rename(doi = 4) %>%
		rename(date_online = 5) %>%
		rename(badge = 6) %>%
		drop_na(ms_num) %>%
		rename(ae_decisions = badge) %>%
		mutate(art_type = "published")  %>%
		mutate(month = month(date), year = year(date)) %>%
		select(ms_num, date, month, year, art_type, ae_decisions) %>%
		arrange(date)


declined_df <- declined_df %>%
			rename(ms_num = 1) %>%
			rename(revision_code = 2) %>%
			rename(art_type = 3) %>%
			rename(chief_ed = 4) %>%
			rename(aer = 5) %>%
			rename(ae_decisions = 9) %>%
			rename(date = 11) %>%
			drop_na(ms_num)  %>%
			mutate(month = month(date), year = year(date)) %>%
			mutate(ae_decisions = "Declined") %>%
			select(ms_num, date, month, year, art_type, ae_decisions) %>%
			arrange(date)

under_review_df <- under_review_df %>%
	rename(ms_num = 1) %>%
	rename(revision_code = 2) %>%
	rename(art_type = 3) %>%
	rename(date = 4) %>%
	rename(aer = 5) %>%
	rename(ae_decisions = 9) %>%
	drop_na(ms_num) %>%
	mutate(month = month(date), year = year(date)) %>%
	mutate(ae_decisions = "UnderReview") %>%
	select(ms_num, date, month, year, art_type, ae_decisions) %>%
	arrange(date)

accepted_noreproduc_df <- accepted_noreproduc_df %>%
rename(ms_num = 1) %>%
rename(revision_code = 2) %>%
rename(art_type = 3) %>%
rename(date = 11) %>%
rename(aer = 6) %>%
rename(ae_decisions = 9) %>%
drop_na(ms_num) %>%
mutate(month = month(date), year = year(date)) %>%
mutate(ae_decisions = "Withdrawn") %>%
select(ms_num, date, month, year, art_type, ae_decisions) %>%
arrange(date)


head(under_review_df)
head(declined_df)
head(accepted_bronze_df)
head(accepted_silver_df)
head(accepted_noreproduc_df)


plot_df <- accepted_silver_df %>% mutate(class = "accept") %>%
	bind_rows(accepted_bronze_df %>% mutate(class = "accept")) %>%
	bind_rows(under_review_df %>% mutate(class = "under_review")) %>%
	bind_rows(declined_df %>% mutate(class = "declined")) %>%
	bind_rows(accepted_noreproduc_df %>% mutate(class = "withdraw")) %>%
	mutate(events = 1)
ggplot(plot_df, aes(x=date, y=ae_decisions)) + geom_point()

ggplot(plot_df %>% filter(ae_decisions != "Declined"), aes(x=date, y=ae_decisions)) + geom_point()

ggplot(plot_df %>% filter(ae_decisions != "Declined"), aes(x=date, y=class)) + geom_point()


###########################################################################
###  More
###########################################################################
accepted_silver_df$cumsum <- seq(1, dim(accepted_silver_df)[1])
accepted_bronze_df$cumsum <- seq(1, dim(accepted_bronze_df)[1])
under_review_df$cumsum <- seq(1, dim(under_review_df)[1])
accepted_noreproduc_df$cumsum  <- seq(1, dim(accepted_noreproduc_df)[1])
declined_df$cumsum <- seq(1, dim(declined_df)[1])

plot_df <- plot_df %>%
	mutate(season = quarter(date, with_year = TRUE)) %>%
	mutate(year_half = case_when(month <= 6 ~ 0,
		month <= 12 ~ 0.5
	)) %>%
	mutate(year_half = year + year_half)


fa <- plot_df %>%
	group_by(ae_decisions, year_half) %>%
	summarize(count = sum(events)) %>%
	ungroup() %>%
	mutate(count_permonth = count /6)

fa$ae_decisions <- factor(fa$ae_decisions, levels = c("Declined", "Withdrawn", "UnderReview", "Bronze", "Silver"), labels = c("Declined", "Withdrawn", "Under Review", "Published: Bronze", "Published: Silver"))


p <- ggplot(fa %>% filter(ae_decisions != "Declined"), aes(x=year_half, y=count_permonth, colour = ae_decisions, group = ae_decisions)) %>%
	+ geom_line() %>%
	+ scale_y_continuous(name = "Articles (per month)", breaks = seq(0,2, 0.25)) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1900, 2025, by = 1)) %>%
	+ scale_colour_brewer(name = "", palette = "Set1") %>%
	+ coord_cartesian(ylim = c(0,1.5), xlim = c(2020.8, 2023.4)) %>%
	+ theme_classic(12) %>%
	+ theme(legend.position = c(0.25, 0.85))

p

### Save plot
ggsave(file.path(write_figures_path, "published_permonth.png"), p,  width = 4, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "published_permonth.pdf"), p,  width = 4, height = 3.5)
ggsave(file.path(write_figures_path, "published_permonth.svg"), p,  width = 4, height = 3.5)

p <- ggplot(fa %>% filter(ae_decisions != "Declined"), aes(x=year_half, y=count_permonth * 6, colour = ae_decisions, group = ae_decisions)) %>%
	+ geom_line() %>%
	+ scale_y_continuous(name = "Articles (per half year)", breaks = seq(0,10, 2)) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1900, 2025, by = 1)) %>%
	+ scale_colour_brewer(name = "", palette = "Set1") %>%
	+ coord_cartesian(ylim = c(0,10), xlim = c(2020.8, 2023.4)) %>%
	+ theme_classic(12) %>%
	+ theme(legend.position = c(0.25, 0.85))

p


### Save plot
ggsave(file.path(write_figures_path, "published_perhalfyear.png"), p,  width = 4, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "published_perhalfyear.pdf"), p,  width = 4, height = 3.5)
ggsave(file.path(write_figures_path, "published_perhalfyear.svg"), p,  width = 4, height = 3.5)





fa <- plot_df %>%
	mutate(ae_decision_reorg = case_when(
			ae_decisions == "Bronze" ~ "Reproducible",
			ae_decisions == "Silver" ~ "Reproducible",
		 	ae_decisions == "Declined" ~ "Declined",
			ae_decisions == "UnderReview" ~ "UnderReview",
			ae_decisions == "Withdrawn" ~ "Withdrawn")) %>%
	group_by(ae_decision_reorg, year_half) %>%
	summarize(count = sum(events)) %>%
	ungroup() %>%
	mutate(count_permonth = count /6) %>%
	rename(ae_decisions = ae_decision_reorg)

fa$ae_decisions <- factor(fa$ae_decisions, levels = c("Declined", "Withdrawn", "UnderReview", "Reproducible"), labels = c("Declined", "Withdrawn", "Under Review", "Reproducible"))


p <- ggplot(fa %>% filter(ae_decisions != "Declined"), aes(x=year_half, y=count_permonth, colour = ae_decisions, group = ae_decisions)) %>%
	+ geom_line() %>%
	+ scale_y_continuous(name = "Articles (per month)", breaks = seq(0,2, 0.25)) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1900, 2025, by = 1)) %>%
	+ scale_colour_brewer(name = "", palette = "Set1") %>%
	+ coord_cartesian(ylim = c(0,1.5), xlim = c(2020.8, 2023.4)) %>%
	+ theme_classic(12) %>%
	+ theme(legend.position = c(0.25, 0.85))

p

### Save plot
ggsave(file.path(write_figures_path, "published_alt_permonth.png"), p,  width = 4, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "published_alt_permonth.pdf"), p,  width = 4, height = 3.5)
ggsave(file.path(write_figures_path, "published_alt_permonth.svg"), p,  width = 4, height = 3.5)

p <- ggplot(fa %>% filter(ae_decisions != "Declined"), aes(x=year_half, y=count_permonth * 6, colour = ae_decisions, group = ae_decisions)) %>%
	+ geom_line() %>%
	+ scale_y_continuous(name = "Articles (per half year)", breaks = seq(0,10, 2)) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1900, 2025, by = 1)) %>%
	+ scale_colour_brewer(name = "", palette = "Set1") %>%
	+ coord_cartesian(ylim = c(0,10), xlim = c(2020.8, 2023.4)) %>%
	+ theme_classic(12) %>%
	+ theme(legend.position = c(0.25, 0.95))

p


### Save plot
ggsave(file.path(write_figures_path, "published_alt_perhalfyear.png"), p,  width = 4, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "published_alt_perhalfyear.pdf"), p,  width = 4, height = 3.5)
ggsave(file.path(write_figures_path, "published_alt_perhalfyear.svg"), p,  width = 4, height = 3.5)




fa <- plot_df %>%
	mutate(season = quarter(date, with_year = TRUE)) %>%
	group_by(class, season) %>%
	summarize(count = sum(events)) %>%
	ungroup()


	ggplot(fa %>% filter(class != "declined"), aes(x=season, y=count, colour = class, group = class)) + geom_line()


mutate(season = case_when(month <= 3 ~ 1,
	month <= 6 ~ 2,
	month <= 9 ~ 3,
	month <= 12 ~ 4
)) %>%
group_by(ae_decisions, year, season) %>%
summarize(count = sum(events)) %>%
ungroup()





plot_df <- accepted_silver_df %>%
	bind_rows(accepted_bronze_df) %>%
	bind_rows(under_review_df) %>%
	bind_rows(declined_df) %>%
	bind_rows(accepted_noreproduc_df) %>%
	mutate(events = 1)


	plot_df$ae_decisions <- factor(plot_df$ae_decisions, levels = c("Declined", "Withdrawn", "UnderReview", "Bronze", "Silver"), labels = c("Declined", "Withdrawn", "Under Review", "Published: Bronze", "Published: Silver"))

	lims2 <- as.POSIXct(strptime(c("2020-10-01 00:00",
	                               "2023-09-01 00:00"),
	                             format = "%Y-%m-%d %H:%M"),
	                    tz = 'UTM')

p <- ggplot(plot_df %>% filter(ae_decisions != "Declined"), aes(x=date, y=cumsum, colour = ae_decisions)) %>%
	+ geom_line() %>%
	+ scale_y_continuous(name = "Cumulative Articles", breaks = seq(0,15, 5)) %>%
	+ scale_x_datetime(name = "Year")%>%
	+ scale_colour_brewer(name = "", palette = "Set1") %>%
	+ coord_cartesian(ylim = c(0,20), xlim = lims2) %>%
	+ theme_classic(12) %>%
	+ theme(legend.position = c(0.25, 0.95))

p

### Save plot
ggsave(file.path(write_figures_path, "cum_plot.png"), p,  width = 4, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "cum_plot.pdf"), p,  width = 4, height = 3.5)
ggsave(file.path(write_figures_path, "cum_plot.svg"), p,  width = 4, height = 3.5)




plot_df <- accepted_silver_df %>%
	bind_rows(accepted_bronze_df) %>%
	mutate(ae_decisions = "Reproducible") %>%
	arrange(date)

	plot_df$cumsum  <- seq(1, dim(plot_df)[1])

plot_df <- plot_df	 %>%
	bind_rows(under_review_df) %>%
	bind_rows(declined_df) %>%
	bind_rows(accepted_noreproduc_df) %>%
	mutate(events = 1)


	plot_df$ae_decisions <- factor(plot_df$ae_decisions, levels = c("Declined", "Withdrawn", "UnderReview", "Reproducible"), labels = c("Declined", "Withdrawn", "Under Review", "Reproducible"))

	p <- ggplot(plot_df %>% filter(ae_decisions != "Declined"), aes(x=date, y=cumsum, colour = ae_decisions)) %>%
		+ geom_line() %>%
		+ scale_y_continuous(name = "Cumulative Articles", breaks = seq(0,15, 5)) %>%
		+ scale_x_datetime(name = "Year")%>%
		+ scale_colour_brewer(name = "", palette = "Set1") %>%
		+ coord_cartesian(ylim = c(0,20), xlim = lims2) %>%
		+ theme_classic(12) %>%
		+ theme(legend.position = c(0.25, 0.95))

	p


	### Save plot
	ggsave(file.path(write_figures_path, "cum_plot_alt.png"), p,  width = 4, height = 3.5, dpi = 600)
	ggsave(file.path(write_figures_path, "cum_plot_alt.pdf"), p,  width = 4, height = 3.5)
	ggsave(file.path(write_figures_path, "cum_plot_alt.svg"), p,  width = 4, height = 3.5)







yup <- plot_df %>%
	arrange(date) %>%
	pivot_wider(names_from = ae_decisions, values_from = events, values_fn = length, values_fill = 0) %>%
	mutate_at(-1,cumsum)


df %>%
  #removing NAs
  .[complete.cases(df),] %>%
  # Arrange by data
  arrange(Date) %>%
  #wide format df with the count of each groups events at each time
  #(some dates have more than on event)(NA of dates mismatch, replace by 0)
  pivot_wider(names_from = Group,names_glue = "{Group}", values_from = Events, values_fn = length, values_fill = 0) %>%
  #changing groups event per date to cumsum
  mutate_at(-1,cumsum) %>%
  # long format
  pivot_longer(cols = -1, names_to = "Groups", values_to = "Cumsum") %>%

  ggplot() +
    geom_line(aes(x = Date, y = Cumsum, linetype = Groups)) +
    labs(y = "Cumulative Frequency", x = "Months")














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
