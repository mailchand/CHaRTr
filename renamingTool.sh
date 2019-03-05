
# Rename files with stone into DDM
find . -type f -exec rename  's/(.*)\/(.*)DDMSvStDitterichSimple(.*)/$1\/$2DDMdSv$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)DDMSvStDitterich(.*)/$1\/$2DDMdSvSt$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)DDMSvStUrgency(.*)/$1\/$2DDMuSvSbSt$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)DDMSvUrgencySimpleSt(.*)/$1\/$2DDMuSvSt$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)DDMSvStCollapse(.*)/$1\/$2DDMcSvSt$3/' {} +

find . -type f -exec rename 's/(.*)\/(.*)DDMSvUrgencySimple(.*)/$1\/$2DDMuSv$3/' {} +

find . -type f -exec rename  's/(.*)\/(.*)DDMSvUrgency(.*)/$1\/$2DDMuSvSb$3/' {} +


find . -type f -exec rename 's/(.*)\/(.*)UGMSvVarintercept(.*)/$1\/$2UGMbSvSb$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)UGMSvintercept(.*)/$1\/$2UGMbSv$3/' {} +

find . -type f -exec rename 's/(.*)\/(.*)UGMintercept(.*)/$1\/$2UGMb$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)UGMSvVarintercept(.*)/$1\/$2UGMbSvSb$3/' {} +



find . -type f -exec rename 's/(.*)\/(.*)DDMSvStCollapse(.*)/$1\/$2DDMcSvSt$3/' {} +


# find . -type f -exec rename -n 's/(.*)\/(.*)DDMSvCollapse(.*)/$1\/$2DDMSvStCollapse$3/' {} +
# find . -type f -exec rename  's/(.*)\/(.*)UGMSvallVar(.*)/$1\/$2UGMallVar$3/' {} +
# find . -type f -exec rename 's/(.*)\/(.*)stone(.*)/$1\/$2DDM$3/' {} +
# find . -type f -exec rename -n 's/(.*)\/(.*)DDMSvUrgencySt(.*)/$1\/$2DDMSvStUrgency$3/' {} +
# find . -type f -exec rename -n 's/(.*)\/(.*)DDMSvUrgencySimpleSt(.*)/$1\/$2DDMSvStUrgencySimple$3/' {} +



find . -type f -exec rename 's/(.*)\/(.*)DDMcSvSt(.*)/$1\/$2cDDMSvSt$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)DDMdSvSt(.*)/$1\/$2dDDMSvSt$3/' {} +
find . -type f -exec rename 's/(.*)\/(.*)DDMdSv(.*)/$1\/$2dDDMSv$3/' {} +
find . -type f -exec rename -n 's/(.*)\/(.*)UGMb(.*)/$1\/$2bUGM$3/' {} +


find . -type f -exec rename 's/(.*)\/(.*)DDMu(.*)/$1\/$2uDDM$3/' {} +



% Change all tibs data


find . -type f -exec rename -n 's/(.*)\/(.*)DDMSvCollapse(.*)/$1\/$2cDDMSv$3/' {} +
