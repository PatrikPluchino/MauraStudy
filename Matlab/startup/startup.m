% WorkSpace
clear all
close all
clc

% Paths
tic
[eye] = convertBlinkTxt2Mat(which('Maura_Blink.txt'));
save('fileMat', 'eye');
toc
