classdef gem_export_obj < handle
% KLASSE ZUR VERWALTUNG VON EXPORTEN, zunaechst mal nur f. timelock
    % * BEISPIEL:
        % pn = jsondecode('["/export/nfs_share/BIOMAG_DATA/Projects/_TEST_DATA_SB/TEST_DATA/Data_Laura/subj_l02_merged","/export/nfs_share/BIOMAG_DATA/Projects/_TEST_DATA_SB/TEST_DATA/Data_Laura/subj_l03_merged","/export/nfs_share/BIOMAG_DATA/Projects/_TEST_DATA_SB/TEST_DATA/Data_Laura/subj_l04_merged","/export/nfs_share/BIOMAG_DATA/Projects/_TEST_DATA_SB/TEST_DATA/Data_Laura/subj_l05_merged"]');
        % OBJ = gem_export_obj('timelock')
        % OBJ.add_data(pn{1})
        % OBJ.add_data(pn{2})
        % OBJ.add_data(pn{3})
        % OBJ.add_data(pn{4})

    %% PROPERTIES
    properties
        type char
        subjects cell
        channels = {}
        time = []
        trials = []

        data cell
    end

    %% METHODS
    methods
        function obj = gem_export_obj(val)
            if nargin == 1
                obj.type = val;
            end
        end

        function add_data(obj, subject_pn)
            if isempty(obj.type), disp('kein type gesetzt!'), return; end
            
            switch obj.type
                case 'timelock'
                    data = gem_return_DATA(subject_pn, 'analysis_time.mat');
                    if isempty(data), disp('konnte nicht gelesen werden'); return;end

                    obj.newsubject(subject_pn, data)

                otherwise
                    fprintf('\t not supported now\n')

            end

        end

        function out = subject_is_new(obj, id);
            %ueberpruefe, ob der subject schon existiert
            out = true;
            if isempty(obj.subjects); return; end
            list = obj.subjectlist();
            if max(ismember(list, id)) > 0
                out = false;
            end

        end

        function newsubject(obj, subject_pn, data)
            subj = struct();
            subj.pn = subject_pn;
            subj.id = gem_return_subject_ID(subject_pn);

            if ~obj.subject_is_new(subj.id)
                fprintf('\tsubject schon vorhanden, ueberspringe ...\n');
                return;
            end

            obj.subjects = [obj.subjects subj];
            obj.data = [obj.data {data}];

            obj.update_time(data);
            obj.update_channels(data);
            obj.update_trials(data);
        end

        function list = subjectlist(obj)
            list = {};
            for i = 1:numel(obj.subjects)
                list{i,1} = obj.subjects{i}.id;

            end
        end

        % UPDATE TIME, CHANNELS und TRIALS

        function update_time(obj, data)
            if isempty(obj.time)
                obj.time = data{1}.time;
            end
        end

        function update_channels(obj, data)
            if isempty(obj.channels)
                obj.channels = data{1}.label;
                return
            end

            channels = data{1}.label;
            if (min(contains(channels, obj.channels)) == 1) && numel(channels) == numel(obj.channels)
                %alles ok
                return
            end

            assignin('base', 'channels1', channels)
            assignin('base', 'channels2', obj.channels)

            %channels sind unterschiedlich!!!
            fprintf('\tupdate channellist: %i > ', max(numel(channels), numel(obj.channels)));
            obj.channels = intersect(obj.channels, channels);
            fprintf('%i\n', numel(obj.channels))
        end

        function update_trials(obj, data)
            if isempty(obj.trials)
                obj.trials = obj.extract_trials(data);
                return
            end
            trials = obj.extract_trials(data);
            if numel(obj.trials) == numel(trials) 
                % alles ok
                return
            end

            fprintf('\tupdate trials: %i > ', max(numel(trials), numel(obj.trials)));
            obj.trials = intersect(obj.trials, trials)
            fprintf('%i\n', numel(obj.chantrialsnels))

        end

        function trials = extract_trials(obj, data)
            trials = [];
            for i = 1:numel(data)
                trials(i) = data{i}.cfg.gem_trialinfo;
            end
        end

        % * EXPORT FUNKTION
        function out = export(obj)
            out = obj.export_to_struct();
        end

        function out = export_to_struct(obj)
            out = [];
            if isempty(obj.data), fprintf('\tobjekt ist noch leer ...\n'); end

            out = struct;
            out.subjects_id = obj.subjectlist();
            out.channels = obj.channels;
            out.time = obj.time;
            out.trials = obj.trials;
            out.type = obj.type;

            out.data = {};

            for i = 1:numel(out.subjects_id)
                tmp = struct();
                tmp.id = out.subjects_id{i};
                tmp.data = {};
                data = obj.data{i};

                for i_trials = 1:numel(obj.trials)
                    trial = obj.trials(i_trials);
                    data_trial = data{i_trials};
                    assert(trial == data_trial.cfg.gem_trialinfo);
                    %gehe sicher, dass die gewaehlten channels korrekt sind
                    pos = find(contains(data_trial.label, out.channels));

                    tmp.data{i_trials}.trial = trial;
                    tmp.data{i_trials}.avg = data_trial.avg(pos);
                    tmp.data{i_trials}.var = data_trial.var(pos);
                    tmp.data{i_trials}.dof = data_trial.dof(pos);
                end

                out.data{i} = tmp;
            end
        end

        function out = export_to_json(obj)
            out = jsonencode(obj.export_to_struct())
        end

        function export_to_file(obj, filename)
            data = obj.export_to_struct();
            gem_save2json(data, filename);
        end

    end
end