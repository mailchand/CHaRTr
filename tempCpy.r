source("chartr-HelperFunctions.r");
availModels = returnListOfModels();
availModels = unname(availModels$modelNames);


models = c('uDDM', 'uDDMSt', 'bUGM','bUGMSt','cDDM','cDDMSt',
        'dDDM','dDDMSt','cfkDDM','cfkDDMSt','cDDMSv', 'DDMSt', 'cDDMSvSz')

models = availModels
subjectDir = 'caseStudy2'
resultDir = 'caseStudy2_Fits'


fnams=dir(subjectDir)
fnams = fnams[c(1,3,4,5)]


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
      print(paste(dest,'-->', src,sep=''));
      
      # file.copy(src, dest);
      
      if(1)
      {
        load(src);
        oldData = out;
        load(dest);
        newData = out;
        
        
        #oldData["timings"] = newData["timings"];
        #oldData["timingLogLst"] = newData["timingLogLst"];
        #oldData["timingLogTxt"] = newData["timingLogTxt"];
        #oldData["rawTimingsTic"] = newData["rawTimingsTic"];
        #oldData["rawTimingsToc"] = newData["rawTimingsToc"];
        #oldData["nparticles"] = newData["nparticles"];
        #oldData["nmc"] = newData["nmc"];
        
        out=newData;
        
        save(out, file=src);
      }
    }
  }
}



