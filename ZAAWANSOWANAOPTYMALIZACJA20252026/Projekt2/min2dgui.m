function min2d_gui
% Minimal GUI – dynamiczne parametry metod optymalizacji
% + MaxIter (globalny)
% + licznik iteracji + powód zakończenia

    %% ===================== FIGURA I OSIE =====================
    fig = uifigure('Name','Minimum f(x,y)', 'Position',[100 100 1000 580]);
    ax = uiaxes(fig, 'Position',[360 60 620 500]);
    title(ax,'Wykres konturowy'); xlabel(ax,'x'); ylabel(ax,'y'); grid(ax,'on');

    %% ===================== FUNKCJA =====================
    uilabel(fig,'Position',[20 530 320 22], ...
        'Text','Funkcja f(x,y):');
    edtF = uieditfield(fig,'text','Position',[20 505 320 28], ...
        'Value','(1-x)^2 + 100*(y-x^2)^2');

    %% ===================== KATEGORIE =====================
    bgCat = uibuttongroup(fig,'Title','Typ metody', ...
        'Position',[20 420 320 90], ...
        'SelectionChangedFcn',@(~,~)onCategoryChanged());

    rbNG = uiradiobutton(bgCat,'Text','Niegradientowe','Position',[10 45 150 22]);
    rbG  = uiradiobutton(bgCat,'Text','Gradientowe','Position',[170 45 120 22]);
    rbND = uiradiobutton(bgCat,'Text','Niedeterministyczne','Position',[10 20 180 22]);
    bgCat.SelectedObject = rbNG;

    %% ===================== METODY =====================
    % Niegradientowe
    bgNGm = uibuttongroup(fig,'Title','Metody niegradientowe', ...
        'Position',[20 290 320 120], ...
        'SelectionChangedFcn',@(~,~)updateMethodParams());
    uiradiobutton(bgNGm,'Text','Hooke-Jeeves','Position',[10 70 200 22]);
    uiradiobutton(bgNGm,'Text','Nelder-Mead','Position',[10 45 200 22]);
    uiradiobutton(bgNGm,'Text','Powell','Position',[10 20 200 22]);
    bgNGm.SelectedObject = bgNGm.Children(2);

    % Gradientowe
    bgGm = uibuttongroup(fig,'Title','Metody gradientowe', ...
        'Position',[20 290 320 120], ...
        'Visible','off', ...
        'SelectionChangedFcn',@(~,~)updateMethodParams());
    uiradiobutton(bgGm,'Text','Fletcher-Reeves','Position',[10 70 200 22]);
    uiradiobutton(bgGm,'Text','DFP','Position',[10 45 200 22]);
    uiradiobutton(bgGm,'Text','BFGS','Position',[10 20 200 22]);
    bgGm.SelectedObject = bgGm.Children(1);

    % Niedeterministyczne
    bgNDm = uibuttongroup(fig,'Title','Metody niedeterministyczne', ...
        'Position',[20 290 320 120], ...
        'Visible','off', ...
        'SelectionChangedFcn',@(~,~)updateMethodParams());
    uiradiobutton(bgNDm,'Text','Algorytm ewolucyjny','Position',[10 60 250 22]);
    bgNDm.SelectedObject = bgNDm.Children(1);

    %% ===================== PANEL PARAMETRÓW METODY =====================
    panelParams = uipanel(fig,'Title','Parametry metody', ...
        'Position',[20 165 320 115]);

    %% ===================== PARAMETRY OGÓLNE =====================
    uilabel(fig,'Position',[20 135 120 22],'Text','Start (x0,y0):');
    edtX0 = uieditfield(fig,'text','Position',[150 135 60 26],'Value','-1');
    edtY0 = uieditfield(fig,'text','Position',[220 135 60 26],'Value','2');

    uilabel(fig,'Position',[20 105 120 22],'Text','Zakres x [a b]:');
    edtXrng = uieditfield(fig,'text','Position',[150 105 160 26],'Value','-2 2');
    uilabel(fig,'Position',[20 75 120 22],'Text','Zakres y [c d]:');
    edtYrng = uieditfield(fig,'text','Position',[150 75 160 26],'Value','-1 3');

    uilabel(fig,'Position',[20 45 120 22],'Text','Epsilon:');
    edtTol = uieditfield(fig,'text','Position',[150 45 160 26],'Value','1e-6');

    % >>> NOWE: Max iteracji
    uilabel(fig,'Position',[20 15 120 22],'Text','Max iteracji:');
    edtMaxIter = uieditfield(fig,'text','Position',[150 15 160 26],'Value','500');

    %% ===================== PRZYCISKI =====================
    uibutton(fig,'Text','Kontury','Position',[360 20 100 30], ...
        'ButtonPushedFcn',@(~,~)plotContours());
    uibutton(fig,'Text','Oblicz minimum','Position',[470 20 140 30], ...
        'ButtonPushedFcn',@(~,~)runAll());

    txtOut = uitextarea(fig,'Position',[620 20 360 30],'Editable','off');

    %% ===================== START =====================
    onCategoryChanged();
    updateMethodParams();
    plotContours();

    %% ===================== CALLBACKI =====================
    function onCategoryChanged()
        bgNGm.Visible = strcmp(bgCat.SelectedObject.Text,'Niegradientowe');
        bgGm.Visible  = strcmp(bgCat.SelectedObject.Text,'Gradientowe');
        bgNDm.Visible = strcmp(bgCat.SelectedObject.Text,'Niedeterministyczne');
        updateMethodParams();
    end

    function method = getMethod()
        switch bgCat.SelectedObject.Text
            case 'Niegradientowe'
                method = bgNGm.SelectedObject.Text;
            case 'Gradientowe'
                method = bgGm.SelectedObject.Text;
            case 'Niedeterministyczne'
                method = bgNDm.SelectedObject.Text;
        end
    end

    %% ===================== DYNAMICZNE PARAMETRY =====================
    function updateMethodParams()
        delete(panelParams.Children);
        method = getMethod();

        y  = 30;   % start niżej (żeby nie wchodzić w tytuł panelu)
        dy = 30;   % odstęp między wierszami

        switch method
            case 'Hooke-Jeeves'
                uilabel(panelParams,'Text','Krok \delta:', 'Position',[10 y 120 22]);
                edt = uieditfield(panelParams,'text', 'Position',[150 y 120 26], 'Value','0.5');
                setappdata(fig,'delta',edt);

            case 'Fletcher-Reeves'
                uilabel(panelParams,'Text','Krok pocz. \alpha:', 'Position',[10 y 120 22]);
                edt = uieditfield(panelParams,'text', 'Position',[150 y 120 26], 'Value','1');
                setappdata(fig,'alpha',edt);

            case 'Algorytm ewolucyjny'
                uilabel(panelParams,'Text','Populacja:', 'Position',[10 y+dy 120 22]);
                edtP = uieditfield(panelParams,'text', 'Position',[150 y+dy 120 26], 'Value','60');

                % Zostawiam "Generacje", ale i tak ograniczamy przez MaxIter.
                uilabel(panelParams,'Text','Generacje:', 'Position',[10 y 120 22]);
                edtG = uieditfield(panelParams,'text', 'Position',[150 y 120 26], 'Value','80');

                setappdata(fig,'pop',edtP);
                setappdata(fig,'gen',edtG);

            otherwise
                uilabel(panelParams,'Text','Brak dodatkowych parametrów.', ...
                    'Position',[10 y 280 22]);
        end
    end

    %% ===================== FUNKCJA I RYSOWANIE =====================
    function fxy = makeF()
        fxy = str2func(['@(x,y)', edtF.Value]);
    end

    function plotContours()
        fxy = makeF();
        xr = sscanf(edtXrng.Value,'%f %f');
        yr = sscanf(edtYrng.Value,'%f %f');
        [X,Y] = meshgrid(linspace(xr(1),xr(2),120), ...
                         linspace(yr(1),yr(2),120));
        Z = arrayfun(fxy,X,Y);
        cla(ax);
        contour(ax,X,Y,Z,30); hold(ax,'on');
        plot(ax,str2double(edtX0.Value),str2double(edtY0.Value),'kx','LineWidth',2);
        hold(ax,'off');
    end

    %% ===================== URUCHOMIENIE METODY =====================
    function runAll()
        fxy = makeF();
        f = @(v) fxy(v(1),v(2));

        x0 = [str2double(edtX0.Value); str2double(edtY0.Value)];
        tol = str2double(edtTol.Value);
        maxIter = str2double(edtMaxIter.Value);

        method = getMethod();

        iters = NaN;
        reason = "—";

        switch method
            case 'Nelder-Mead'
                % licznik iteracji: OutputFcn
                lastIter = 0;
                stopByMax = false;

                opts = optimset( ...
                    'MaxIter', maxIter, ...
                    'TolX', tol, ...
                    'TolFun', tol, ...
                    'Display','off', ...
                    'OutputFcn', @outFminsearch);

                [xmin,fmin,exitflag,output] = fminsearch(f,x0,opts);
                iters = output.iterations;

                if stopByMax || exitflag == 0
                    reason = "osiągnięto max iteracji";
                elseif exitflag == 1
                    reason = "osiągnięto tolerancję";
                else
                    reason = "zakończono (inne)";
                end

            case 'DFP'
                lastIter = 0;
                stopByMax = false;

                opts = optimoptions('fminunc', ...
                    'Algorithm','quasi-newton', ...
                    'HessUpdate','dfp', ...
                    'MaxIterations',maxIter, ...
                    'OptimalityTolerance',tol, ...
                    'StepTolerance',tol, ...
                    'Display','off', ...
                    'OutputFcn', @outFminunc);

                [xmin,fmin,exitflag,output] = fminunc(f,x0,opts);
                iters = output.iterations;

                if stopByMax || exitflag == 0
                    reason = "osiągnięto max iteracji";
                elseif exitflag > 0
                    reason = "osiągnięto tolerancję";
                else
                    reason = "zakończono (inne)";
                end

            case 'BFGS'
                lastIter = 0;
                stopByMax = false;

                opts = optimoptions('fminunc', ...
                    'Algorithm','quasi-newton', ...
                    'HessUpdate','bfgs', ...
                    'MaxIterations',maxIter, ...
                    'OptimalityTolerance',tol, ...
                    'StepTolerance',tol, ...
                    'Display','off', ...
                    'OutputFcn', @outFminunc);

                [xmin,fmin,exitflag,output] = fminunc(f,x0,opts);
                iters = output.iterations;

                if stopByMax || exitflag == 0
                    reason = "osiągnięto max iteracji";
                elseif exitflag > 0
                    reason = "osiągnięto tolerancję";
                else
                    reason = "zakończono (inne)";
                end

            case 'Hooke-Jeeves'
                delta = str2double(getappdata(fig,'delta').Value);
                [xmin,fmin,iters,reason] = hookeJeeves2D(f,x0,delta,tol,maxIter);

            case 'Fletcher-Reeves'
                alpha0 = str2double(getappdata(fig,'alpha').Value);
                [xmin,fmin,iters,reason] = fletcherReeves2D(f,x0,alpha0,tol,maxIter);

            case 'Powell'
                [xmin,fmin,iters,reason] = powell2D(f,x0,tol,maxIter);

            case 'Algorytm ewolucyjny'
                pop = str2double(getappdata(fig,'pop').Value);
                genFromPanel = str2double(getappdata(fig,'gen').Value);
                gen = min(genFromPanel, maxIter); % ograniczamy maxIter

                xr = sscanf(edtXrng.Value,'%f %f');
                yr = sscanf(edtYrng.Value,'%f %f');

                gaOut = [];
                opts = optimoptions('ga', ...
                    'PopulationSize',pop, ...
                    'MaxGenerations',gen, ...
                    'Display','off', ...
                    'OutputFcn', @outGA);

                [xmin,fmin,exitflag,output] = ga(@(v)fxy(v(1),v(2)),2,[],[],[],[], ...
                                 [xr(1),yr(1)],[xr(2),yr(2)], ...
                                 [],opts);
                xmin = xmin(:);

                iters = output.generations;
                if exitflag == 0 || output.generations >= gen
                    reason = "osiągnięto max iteracji";
                else
                    reason = "zakończono (GA)";
                end
        end

        plotContours();
        hold(ax,'on');
        plot(ax,xmin(1),xmin(2),'ro','LineWidth',2);
        hold(ax,'off');

        txtOut.Value = sprintf('%s: xmin=(%.6g, %.6g), f=%.6g | iter=%d | %s', ...
            method,xmin(1),xmin(2),fmin,round(iters),reason);

        % -------- OutputFcn callbacks --------
        function stop = outFminsearch(x, optimValues, state)
            stop = false;
            if strcmp(state,'iter')
                lastIter = optimValues.iteration;
                if lastIter >= maxIter
                    stop = true;
                    stopByMax = true;
                end
            end
        end

        function stop = outFminunc(x, optimValues, state)
            stop = false;
            if strcmp(state,'iter')
                lastIter = optimValues.iteration;
                if lastIter >= maxIter
                    stop = true;
                    stopByMax = true;
                end
            end
        end

        function [state, options, optchanged] = outGA(options, state, flag)
            optchanged = false;
            if strcmp(flag,'iter')
                gaOut = state.Generation; %#ok<NASGU>
            end
        end
    end
end

%% ===================== HOOKE-JEEVES (2D) =====================
function [x,fval,iter,reason] = hookeJeeves2D(f,x0,delta,tol,maxIter)
    x = x0(:);
    fcur = f(x);
    alpha = 2;
    shrink = 0.5;

    iter = 0;
    reason = "—";

    while iter < maxIter
        iter = iter + 1;

        [x_new,f_new] = exploratory(f,x,delta,fcur);

        if f_new < fcur
            x_pat = x_new + alpha*(x_new - x);
            f_pat = f(x_pat);

            if f_pat < f_new
                x = x_pat;
                fcur = f_pat;
            else
                x = x_new;
                fcur = f_new;
            end
        else
            delta = delta*shrink;
            if delta <= tol
                reason = "osiągnięto tolerancję";
                break;
            end
        end
    end

    if iter >= maxIter && reason == "—"
        reason = "osiągnięto max iteracji";
    end

    fval = fcur;
end

function [x_new,f_new] = exploratory(f,x,delta,fcur)
    x_new = x;
    f_new = fcur;

    for i = 1:2
        xt = x_new; xt(i) = xt(i) + delta;
        ft = f(xt);
        if ft < f_new
            x_new = xt; f_new = ft;
        else
            xt = x_new; xt(i) = xt(i) - delta;
            ft = f(xt);
            if ft < f_new
                x_new = xt; f_new = ft;
            end
        end
    end
end

%% ===================== FLETCHER-REEVES (2D) =====================
function [x,fval,iter,reason] = fletcherReeves2D(f,x0,alpha0,tol,maxIter)
    x = x0(:);
    g = numGrad(f,x);
    d = -g;

    iter = 0;
    reason = "—";

    while iter < maxIter
        iter = iter + 1;

        if norm(g) <= tol
            reason = "osiągnięto tolerancję";
            break;
        end

        % Backtracking Armijo
        t = alpha0;
        fx = f(x);
        c1 = 1e-4;

        while f(x + t*d) > fx + c1*t*(g'*d)
            t = 0.5*t;
            if t < 1e-12
                break;
            end
        end

        x_new = x + t*d;
        g_new = numGrad(f,x_new);

        beta = (g_new'*g_new) / max(g'*g, 1e-30);
        d = -g_new + beta*d;

        if norm(x_new - x) <= tol
            x = x_new; g = g_new;
            reason = "osiągnięto tolerancję";
            break;
        end

        x = x_new;
        g = g_new;
    end

    if iter >= maxIter && reason == "—"
        reason = "osiągnięto max iteracji";
    end

    fval = f(x);
end

function g = numGrad(f,x)
    h = 1e-6;
    g = zeros(size(x));
    for i = 1:numel(x)
        xp = x; xm = x;
        xp(i) = xp(i) + h;
        xm(i) = xm(i) - h;
        g(i) = (f(xp) - f(xm)) / (2*h);
    end
end

%% ===================== POWELL (2D) =====================
function [x,fval,iter,reason] = powell2D(f,x0,tol,maxIter)
    x = x0(:);
    D = eye(2);

    iter = 0;
    reason = "—";

    while iter < maxIter
        iter = iter + 1;
        x_start = x;
        f_start = f(x);

        for i = 1:2
            d = D(:,i);
            x = lineSearch1D(f,x,d);
        end

        d_new = x - x_start;
        if norm(d_new) < tol || abs(f_start - f(x)) < tol
            reason = "osiągnięto tolerancję";
            break;
        end

        D(:,1) = D(:,2);
        D(:,2) = d_new / max(norm(d_new), 1e-30);
    end

    if iter >= maxIter && reason == "—"
        reason = "osiągnięto max iteracji";
    end

    fval = f(x);
end

function xbest = lineSearch1D(f,x,d)
    a = -1; b = 1;
    phi = (sqrt(5)-1)/2;

    for k = 1:10
        c = a + (1-phi)*(b-a);
        e = a + phi*(b-a);

        if f(x + c*d) < f(x + e*d)
            b = e;
        else
            a = c;
        end
    end

    alpha = (a+b)/2;
    xbest = x + alpha*d;
end
