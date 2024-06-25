% Exp2_lpips.m
%
% Authors: Edward Vessel
% Last modified: 2023-06-16
% Description: analysis and plots for analysis of lpips content loss scores, for Exp. 2
% Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("Experiment_2")

% Self-relevance and aesthetic appeal, Exp. 2
% Script for analysis of lpips data 



%dataDir = ('');
%cd(dataDir);

%% read in data

rawDatafile = 'Dataratings_Final.csv';

dataTable = readtable(rawDatafile,'Delimiter',';');

%% compute subject averages and centered data

subjList.raw = unique(dataTable.Subj);
nSubj = length(subjList.raw);
nTrials = length(unique(dataTable.Trial));

rawData.aeB1 = reshape(dataTable.b1,[nTrials nSubj]) ./ 100; %rescaling to between 0 - 1
rawData.aeB2 = reshape(dataTable.b2,[nTrials nSubj]) ./ 100; %rescaling to between 0 - 1
rawData.srB3 = reshape(dataTable.b3,[nTrials nSubj]) ./ 100; %rescaling to between 0 - 1
rawData.faB4 = (3 - reshape(dataTable.b4,[nTrials nSubj])) ./ 2; %flipping and rescaling to between 0 - 1

subjAvg.aeB1 = mean(rawData.aeB1);
subjAvg.aeB2 = mean(rawData.aeB2);
subjAvg.srB3 = mean(rawData.srB3);
subjAvg.faB4 = mean(rawData.faB4);

centData.aeB1 = rawData.aeB1 - (ones(nTrials,1) * subjAvg.aeB1) ;
centData.aeB2 = rawData.aeB2 - (ones(nTrials,1) * subjAvg.aeB2) ;
centData.srB3 = rawData.srB3 - (ones(nTrials,1) * subjAvg.srB3) ;
centData.faB4 = rawData.faB4 - (ones(nTrials,1) * subjAvg.faB4) ;

%% get condition types and compute subjectwise condition averages

condNamesOrig = {'SR' 'NR' 'GA' 'RA'};
condNamesNew  = {'SR' 'OR' 'CG' 'RA'};

nCond = length(condNamesOrig);
condKey = reshape(dataTable.Cat,[nTrials nSubj]);
trialsPerCond = nTrials / nCond;

for c = 1:nCond
    condData.aeB1cent(:,:,c) = reshape(centData.aeB1(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condAvgBySubj.aeB1cent(:,c) = mean(squeeze(condData.aeB1cent(:,:,c)));
    condAvg.aeB1cent(c) = mean(condAvgBySubj.aeB1cent(:,c));
    
    condData.aeB2cent(:,:,c) = reshape(centData.aeB2(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condAvgBySubj.aeB2cent(:,c) = mean(squeeze(condData.aeB2cent(:,:,c)));
    condAvg.aeB2cent(c) = mean(condAvgBySubj.aeB2cent(:,c));
    
    condData.srB3cent(:,:,c) = reshape(centData.srB3(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condAvgBySubj.srB3cent(:,c) = mean(squeeze(condData.srB3cent(:,:,c)));
    condAvg.srB3cent(c) = mean(condAvgBySubj.srB3cent(:,c));
    
    condData.faB4cent(:,:,c) = reshape(centData.faB4(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condAvgBySubj.faB4cent(:,c) = mean(squeeze(condData.faB4cent(:,:,c)));
    condAvg.faB4cent(c) = mean(condAvgBySubj.faB4cent(:,c));
    
    condData.aeB1raw(:,:,c) = reshape(rawData.aeB1(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condData.aeB2raw(:,:,c) = reshape(rawData.aeB2(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condData.srB3raw(:,:,c) = reshape(rawData.srB3(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condData.faB4raw(:,:,c) = reshape(rawData.faB4(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    
end





%% Analysis by Similarity Ratings bt style and artwork

% load in similarity data: (higher values are MORE content loss)
load('lpips_data.mat');

% create subject-specific lists of similarity data for SR and OR lists
% need list of matched subject pairs
% subject pairs are always matched by even-odd!

for s = 1:nSubj
   subjList.SR(s) = str2num(subjList.raw{s}(2:3));
   subjList.OR(s) = subjList.SR(s) + (mod(subjList.SR(s),2) *2 - 1);
end

% create a trialwise list of lpips
for r = 1:(nSubj*nTrials)
    thisSubj = str2num(dataTable.Subj{r}(2:3));
    thisImgName = dataTable.Imgname(r);
   switch dataTable.Cat{r}
       case 'SR'
           thisImgNum = str2num(dataTable.Imgname{r}(7:8));
           sSubj = str2num(dataTable.Imgname{r}(2:3));
           if sSubj ~= thisSubj
               disp(['Matching ERROR r ',int2str(r)]);
           end
           lpips_Long(r) = lpips.SRAll(thisImgNum,sSubj);
       case 'NR'
           thisImgNum = str2num(dataTable.Imgname{r}(7:8));
           oSubj = str2num(dataTable.Imgname{r}(2:3));
           if oSubj ~= thisSubj+(mod(thisSubj,2) * 2 -1)
               disp(['Matching ERROR r ',int2str(r)]);
           end
           lpips_Long(r) = lpips.SRAll(thisImgNum,oSubj);
       case 'GA'
           thisImgNum = str2num(dataTable.Imgname{r}(3:4));
           lpips_Long(r) = lpips.CG(thisImgNum);
       case 'RA'
           lpips_Long(r) = NaN;
   end
   
end
dataTable.lpips = lpips_Long';

%reshape data
rawData.lpips = reshape(lpips_Long,[nTrials nSubj]);

%now, grab by condition, compute average lpips
for c = 1:nCond
    condData.lpips(:,:,c) = reshape(rawData.lpips(strcmp(condKey,condNamesOrig{c})),[trialsPerCond nSubj]);
    condAvgBySubj.lpips(:,c) = mean(squeeze(condData.lpips(:,:,c)));
    condAvg.lpips(c) = mean(condAvgBySubj.lpips(:,c));
end

shft = 0.15;

figure;
hold;
hndl.sr = plot([1:nSubj]-shft,condData.lpips(:,:,1),'ro');
hndl.or = plot([1:nSubj]+shft,condData.lpips(:,:,2),'bo');
hndl.ga = plot(nSubj+1,condData.lpips(:,1,3),'ko');
set(gca,'Xtick',[1:nSubj]);
xlabel('Participant');
ylabel('LPIPS');
legend([hndl.sr(1) hndl.or(1) hndl.ga(1)],{'self-relev' 'other-relev' 'control gen'});
axis([0 nSubj+2 0 1.5]);
set(gcf,'Color',[1 1 1]);
text(-2,1.25,'greater loss','Rotation',90);
text(-2,.05,'less loss','Rotation',90);


figure;
hold;
plot(condAvgBySubj.lpips(:,[1:2])','ko--','MarkerSize',4);
plot([1:3],condAvg.lpips(1:3),'g+','LineWidth',4,'MarkerSize',16);
set(gca,'Xtick',[1:3]);
set(gca,'XtickLabel',condNamesNew(1:3));
axis([0.5 3.5 0.4 0.7]);
xlabel('Condition');
ylabel('Average LPIPS');
set(gcf,'Color',[1 1 1]);
text(0.6,0.65,'greater loss','Rotation',90);
text(0.6,0.405,'less loss','Rotation',90);

% Paired t-test for difference bt SR and OR subject means
[condAvgBySubj.lpipsT.h,condAvgBySubj.lpipsT.p,condAvgBySubj.lpipsT.ci,condAvgBySubj.lpipsT.stats]  = ttest(condAvgBySubj.lpips(:,1),condAvgBySubj.lpips(:,2),'tail','both');
% mean and 95% CI

%cohen's d
condAvgBySubj.lpipsT.cohD = -diff(mean(condAvgBySubj.lpips(:,[1:2]))) / sqrt(mean(var(condAvgBySubj.lpips(:,[1:2]))))

% Get image names
imgNames.allTrials = reshape(dataTable.Imgname,[nTrials nSubj]);
imgNames.SR = reshape(imgNames.allTrials(strcmp(condKey,condNamesOrig{1})),[trialsPerCond,nSubj]); % NOT sorted
imgNames.OR = reshape(imgNames.allTrials(strcmp(condKey,condNamesOrig{2})),[trialsPerCond,nSubj]); % NOT sorted
imgNames.CG = sort(imgNames.allTrials(strcmp(condKey(:,1),condNamesOrig{3}),1)); %SORTED
imgNames.RA = sort(imgNames.allTrials(strcmp(condKey(:,1),condNamesOrig{4}),1)); %SORTED


% big scatter plot with all SR trials and all OR trials vs lpips (centered
% data?)
figure;

scatter(reshape(condData.lpips(:,:,1),[nSubj*trialsPerCond,1]),reshape(condData.aeB1cent(:,:,1),[nSubj*trialsPerCond,1]));
hold;
scatter(reshape(condData.lpips(:,:,2),[nSubj*trialsPerCond,1]),reshape(condData.aeB1cent(:,:,2),[nSubj*trialsPerCond,1]));

[lpips.corr.AllSRcent.r,lpips.corr.AllSRcent.p] = corr(reshape(condData.lpips(:,:,1),[nSubj*trialsPerCond,1]),reshape(condData.aeB1cent(:,:,1),[nSubj*trialsPerCond,1]));
[lpips.corr.AllORcent.r,lpips.corr.AllORcent.p] = corr(reshape(condData.lpips(:,:,2),[nSubj*trialsPerCond,1]),reshape(condData.aeB1cent(:,:,2),[nSubj*trialsPerCond,1]));
legend({['SR  r=',num2str(lpips.corr.AllSRcent.r,2)] ['OR  r=',num2str(lpips.corr.AllORcent.r,2)]});
xlabel('LPIPS');
ylabel('Aesthetic Rating (centered)');
title('Trialwise LPIPS vs Rating, SR and OR');
set(gcf,'Color',[1 1 1]);


lpips.corr.AllSRraw = corr(reshape(condData.lpips(:,:,1),[nSubj*trialsPerCond,1]),reshape(condData.aeB1raw(:,:,1),[nSubj*trialsPerCond,1]));
lpips.corr.AllORraw = corr(reshape(condData.lpips(:,:,2),[nSubj*trialsPerCond,1]),reshape(condData.aeB1raw(:,:,2),[nSubj*trialsPerCond,1]));


