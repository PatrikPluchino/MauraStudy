% Cambio Path
pathNew = which('ms.m');
index = strfind(pathNew, filesep);
pathNew = pathNew(1 : index(end));

cd([pathNew]);

% Pulizia Workspace
clear all
close all
clc
