# one-svm provier stat feature-----------------------------------------------------------
svm_provider_stat_result <- svm_df_parallel(splited_df_provider, 
                                   group_name = quos(provider_type), 
                                   feature_name = quo(stat),
                                   nu = nu_range,
                                   leie = leie,
                                   cluster = cluster)


# One-svm provider hcpcs feature ------------------------------------------
svm_provider_hcpcs_result <- svm_df_parallel(splited_df_provider,
                                    group_name = quos(provider_type),
                                    feature_name = quo(hcpcs),
                                    nu = nu_range,
                                    leie = leie,
                                    cluster = cluster)

print("svm provider split completed")
# One-svm provider-mesh stat feature --------------------------------------


svm_providermesh_stat_result <- svm_df_parallel(splited_df_providermesh,
                                       group_name = quos(provider_type, mesh),
                                       feature_name = quo(stat),
                                       nu = nu_range,
                                       leie = leie,
                                       cluster = cluster)


# one-svm provider mesh hcpcs feature -------------------------------------


svm_providermesh_hcpcs_result <- svm_df_parallel(splited_df_providermesh,
                                       group_name = quos(provider_type, mesh),
                                       feature_name = quo(hcpcs),
                                       nu = nu_range,
                                       leie = leie,
                                       cluster = cluster)


print("svm provider mesh split completed")
# one-svm mesh stat feature -----------------------------------------------


svm_mesh_stat_result <- svm_df_parallel(splited_df_mesh,
                               group_name = quos(mesh),
                               feature_name = quo(stat),
                               nu = nu_range,
                               leie = leie,
                               cluster = cluster)


# one-svm mesh stat feature -----------------------------------------------


svm_mesh_hcpcs_result <- svm_df_parallel(splited_df_mesh,
                                group_name = quos(mesh),
                                feature_name = quo(hcpcs),
                                nu = nu_range,
                                leie = leie,
                                cluster = cluster)

print("svm mesh split completed")