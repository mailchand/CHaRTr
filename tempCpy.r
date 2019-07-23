models = c('uDDM', 'uDDMSt', 'bUGM','bUGMSt','cDDM','cDDMSt',
           'dDDM','dDDMSt','cfkDDM','cfkDDMSt','cDDMSv', 'DDMSt', 'cDDMSvSz')

subjectDir = 'caseStudy2'
resultDir = 'caseStudy2_Fits'


fnams=dir(subjectDir)
fnams = fnams[c(1,2,3,4,5)]



srcLetters = letters[1:5];
destLetters = letters[6:10];


for(subject in fnams)
{
  for(m in models)
  {
    for(i in seq(1,5))
    {
      src = paste(resultDir,'/',subject,'-',m,'-',srcLetters[i],sep='');
      dest = paste(resultDir,'/',subject,'-',m,'-',destLetters[i],sep='');
      file.copy(src, dest);
      print(src)
      print(dest)
    }
  }
}