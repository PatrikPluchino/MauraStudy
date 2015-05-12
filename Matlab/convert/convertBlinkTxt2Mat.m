function [eye, RAW] = convertBlinkTxt2Mat(input, checkSave)
% Convert the Blink idf-converted file into a .mat struct.
%
%   Syntax:
%          [eye, RAW] = convertBlinkTxt2Mat(input, checkSave)
%
%   Parameters:
%           input                   file name of the blink idf-converted
%                                   file.
%                                   The file exension allow are:
%                                       - xls
%                                       - xlsx
%                                       - txt
%                                       - mat
%           checkSave               true if you want to save a .mat copy of
%                                   the raw data.
%
%   Return values:
%           eyes                    Struct of the event information for
%                                   both eyes. The tree of the structure:
%
%                              > info:     file, version, sampleRate, date
%                              > events:   trial, number, start, description
%                              > left:     blink:     number, start, end, duration, trial
%                                          saccade:   number, start, end, duration, location, start (x, y), stop (x, y), amplitude, speed (peak (to, from), average), acceleration (peak, average), deceleration (peak, average), trial
%                                          fixations: number, start, end, duration, location, dispersion, plane, pupil, trial
%                              > right:    blink:     number, start, end, duration, trial
%                                          saccade:   number, start, end, duration, location, start (x, y), stop (x, y), amplitude, speed (peak (to, from), average), acceleration (peak, average), deceleration (peak, average), trial
%                                          fixations: number, start, end, duration, location, dispersion, plane, pupil, trial
%
%	Author: Filippo M.  12/05/2015


try
    % Nargin
    if nargin < 2
        checkSave = false;
    end
    if nargin < 1
        [RAW] = idfImport();
    else
        % Get RAW
        if isstruct(input)
            [RAW] = input;
        else
            [RAW] = idfImport(input, checkSave);
        end
    end
    
    % Initialization Eye Struct
    eye = struct(...
        'blink', struct('unit', 'ms', 'trial', [], 'subject', [], 'eyeSide', [], 'number', [], 'start', [], 'end', [], 'duration', [], ...
        'numerosity', [], 'disposition', [], 'detailed_disposition', [], ...
        'stimulus', struct('color', [], 'name', [], 'start', [], 'end', [])));
    
    [rRAW, cRAW] = size(RAW);
    
    if ((cRAW ~= 0)  && (rRAW ~= 0))
        
        for icRAW = 1 : cRAW
            if strfind(RAW{1, icRAW}, 'Trial')
                eye.blink.stimulus.name = RAW((2:end), icRAW);
            elseif strfind(RAW{1, icRAW}, 'Subject')
                eye.blink.subject = fix(str2double(RAW((2:end), icRAW)));
            elseif strfind(RAW{1, icRAW}, 'Color')
                eye.blink.stimulus.color = RAW((2:end), icRAW);
            elseif strfind(RAW{1, icRAW}, 'Stimulus')
                eye.blink.stimulus.name = RAW((2:end), icRAW);
            elseif strfind(RAW{1, icRAW}, 'Start Time')
                eye.blink.stimulus.start = str2double(RAW((2:end), icRAW));
            elseif strfind(RAW{1, icRAW}, 'End Time')
                eye.blink.stimulus.end = str2double(RAW((2:end), icRAW));
            elseif strfind(RAW{1, icRAW}, 'Blink Start')
                eye.blink.start = str2double(RAW((2:end), icRAW));
            elseif strfind(RAW{1, icRAW}, 'Blink Duration')
                eye.blink.duration = str2double(RAW((2:end), icRAW));
            elseif strfind(RAW{1, icRAW}, 'Blink End')
                eye.blink.end = str2double(RAW((2:end), icRAW));
            elseif strfind(RAW{1, icRAW}, 'Eye L/R')
                eye.blink.eyeSide = RAW((2:end), icRAW);
            elseif strfind(RAW{1, icRAW}, 'Number')
                eye.blink.number = str2double(RAW((2:end), icRAW));
            end
        end
        
        for irRAW = 1 : rRAW
            if strfind(RAW{1, icRAW}, 'Trial')
            end
            
        end
        
    else
        disp('Can''t read the file, check the file extension.');
    end
    
catch ME; if (exist('saveMException.m', 'file')); saveMException(ME); end; end


function [matrix] = cellString2Matrix(cellString)
% --
%
%   Syntax:
%          [eye, RAW] = convertBlinkTxt2Mat(input, checkSave)
%
%   Parameters:
%           --
%
%   Return values:
%           --
%
%	Author: Filippo M.  12/05/2015


try
    [nRow, nColoumn] = size(cellString)
    
    matrix = NaN(nRow, 1);
    
    for iRow = 1 : nRow
        matrix(iRow, 1) = str2double(cellString{iRow, 1});
    end
    
catch ME; if (exist('saveMException.m', 'file')); saveMException(ME); end; end
