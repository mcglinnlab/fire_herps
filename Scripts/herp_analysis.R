
library(mobr)

ponds = read.csv('./historicdata.csv')
ponds = as.matrix(ponds)
minnow_hx = ifelse(is.na(ponds), 0,ponds)
ponds = as.data.frame (ponds)

levels(ponds$ff_91_00)
ponds$ff_91_00 = factor(ponds$ff_91_00, levels(ponds$ff_91_00)[c(5, 1:4)])

herps = read.csv('./All Census methods-3d.csv')
head(herps)
herps$Pond = as.factor(herps$Pond)
herps$sample_id = with(herps, paste(Pond, Date, sep="_"))


herp_calls = subset(herps, Method == 'Calling Census')
herp_traps = subset(herps, Method == 'Minnow Trapping')


head(herp_calls)

comm = with(herp_calls, tapply(Species_Code, list(sample_id, Species_Code), length))
comm = with(herp_traps, tapply(Species_Code, list(sample_id, Species_Code), length))
  

comm_ponds = aggregate(herp_calls, by = list(herp_calls$sample_id), function(x) x[1])
comm_ponds = aggregate(herp_traps, by = list(herp_traps$sample_id), function(x) x[1])

comm_ponds = comm_ponds[ , -1]
ponds = merge(ponds, comm_ponds, by.x = "ï..Pond", by.y = "Pond")
ponds = ponds[order(ponds$sample_id), ]

all.equal(as.character(ponds$sample_id), row.names(comm))
row.names(ponds) = ponds$sample_id
names(ponds) = c(names(ponds)[1:2], 'y', 'x', names(ponds)[5:21])


calls_mob_in = make_mob_in(comm, ponds, latlong =T)
calls_mob_in

calls_stats = get_mob_stats(calls_mob_in, group_var = "Burn.Year")
calls_deltas = get_delta_stats(calls_mob_in, group_var = "Burn.Year", 
                               ref_group = "8", type = 'discrete')
                               
pdf('./figs/calls_stats.pdf', height = 7*0.5)
plot(calls_stats)
dev.off()

pdf('./figs/traps_stats.pdf', height = 7*0.5)
plot(calls_stats)
dev.off()

pdf('./figs/calls_deltas.pdf')
plot(calls_deltas, trt_group = "0", ref_group = "8")
dev.off()

overlap_effects(calls_deltas, trt_group = "0")
