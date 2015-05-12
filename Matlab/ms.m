% Cambio Path
pathNew = which('ms.m');
index = strfind(pathNew, filesep);
pathNew = pathNew(1 : index(end));

addpath(genpath(pathNew));

cd([pathNew]);

% Pulizia Workspace
clear all
close all
clc
