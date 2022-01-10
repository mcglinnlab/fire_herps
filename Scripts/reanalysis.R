library(mobr)

env = read.csv('./data/Master_env_file.csv')

herps = read.csv('./data/historic/All_census_methods_hx_modern.csv')

herps$Pond = as.factor(herps$Pond)
herps$sample_id = with(herps, paste(Pond, Date, sep="_"))


herp_calls = subset(herps, Method == 'Calling Census')
herp_traps = subset(herps, Method == 'Minnow Trapping')

## analysis choice ---------------------------------------------
## analyze either the call data 
comm = with(herp_calls, tapply(Species_Code, list(sample_id, Species_Code), length))
comm_ponds = aggregate(herp_calls, by = list(herp_calls$sample_id), function(x) x[1])
## or the trap data
comm = with(herp_traps, tapply(Species_Code, list(sample_id, Species_Code), length))
comm_ponds = aggregate(herp_traps, by = list(herp_traps$sample_id), function(x) x[1])
## --------------------------------------------------------------

comm = ifelse(is.na(comm), 0, comm)
comm_ponds = comm_ponds[ , -1]

env_ponds = merge(env, comm_ponds, by.x = "Pond", by.y = "Pond")
env_ponds = env_ponds[order(env_ponds$sample_id), ]

all.equal(as.character(env_ponds$sample_id), row.names(comm))
row.names(env_ponds) = env_ponds$sample_id

mob_in = make_mob_in(comm, env_ponds, coord_names = c('X', 'Y'), latlong = T)
mob_in

stats = get_mob_stats(mob_in, group_var = "ff_91_00")
plot(stats)


deltas = get_delta_stats(mob_in, group_var = "Burn.Year",
                         type = 'continuous', n_perm = 500)
deltas = get_delta_stats(mob_in, group_var = "Burn.Year",
                         type = 'discrete', ref_group = 0, n_perm = 500)

plot(deltas, trt_group = 8, ref_group = 0)


deltas = get_delta_stats(mob_in, group_var = "ff_91_00",
                         type = 'continuous', n_perm = 250)


