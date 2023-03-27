%% read template image (AAL2)
path = 'AAL2/';
maskvol = spm_vol([path, 'raal2.nii']);
maskAAL = spm_read_vols(maskvol); %% [53, 63, 46]

%% read subject contrast map
% path = '/share/inspurStorage/home1/luhan/NMF/';
%V = spm_vol([path, 'angry_neutral1.nii']);
V = spm_vol('angry_neutral.nii');
[con1, XYZ] = spm_read_vols(V);
AAL_brain = sum(sum(sum((maskAAL <= 94 & maskAAL > 0))));

 %% read factors for angry vs neutral
 % path = '/share/inspurStorage/home1/luhan/NMF/WH_19/';
 % load([path, 'angry_positive_W.mat']);
 load('angry_positive_W.mat');

 %% 
location_need=find(maskAAL <= 94 & maskAAL > 0);
 x0 = zeros(53,63,46);
 x0(location_need) = W2(:,2);
 V.fname = 'ap2_0315.nii';
 spm_write_vol(V,x0);
 
 
 
 
 
 


