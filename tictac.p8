pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
-- state def./constants
-- mejorar sombras
-- agregar calculo de nivel y barra gui
-- ajustar curvas diff y rate
-- penalizar oportunidad desaprovechada
-- color barra ai y timer que cambie/parpadee
-- efecto:prob que aumenta de que ficha se mueva de su sitio
-- probar con alguien,anadir efectos,anadir menu,publicar
gs=nil -- game state
ms=nil -- menu state
-- implicit game state: variables that can be fully derived from
-- 	the current game state, cached here and globally accessible.
--	they are mostly used by the game drawing routines.
-- 	should be written by update_gs and read only by everyone else.
--  given lua's behaviour, this list doesn't even need to be
--  here, but i'm using it as a reminder of the things that are
--  computed by update_gs that are used somewhere else
current_move=nil
new_board=nil
player_win_line=nil
cpu_win_line=nil
new_phase=nil
-- constants
tile_side=33
screen_side=128
tile_off_x=(screen_side-tile_side*3)/2
tile_off_y=(screen_side-tile_side*3)/4
timer_off_x=tile_off_x
timer_off_y=tile_side*3+tile_off_y+3
timer_height=12
cursor_off=tile_side/12
cursor_side=tile_side-2*cursor_off
l_sidebar_x=1
r_sidebar_x=tile_off_x+tile_side*3+2
sidebar_y=tile_off_y
sidebar_h=tile_side*3
sidebar_w=tile_off_x-4
player_mark="❎"
cpu_mark="🅾️"
initial_rate=1
rate_coeff=1.075
diff_inc=11
phases={
	"player","cpu","lost","init",
	"tie_cpu","tie_player","won"
}
m_phases={"main","playing"} -- menu phases
lost_cooldown=40
tie_cooldown=20
won_cooldown=20
error=0
control_rz=0 -- control return to zero
max_blinky_off=tile_side*0.2
blinky_off_speed=0.5
max_timer=255
base_score_per_level=2500
score_digits=5
digit_sprite_base=0x70
score_lsd_x=2
score_lsd_y=10
cursor_dis_color=6
cursor_a_color=7
cursor_b_color=14
shadow_brush=5
x_brush=4
o_brush=3
hi_brush=6 -- brush for drawing highlights
title_brush_1=7
title_brush_2=10
piece_shadows=true
max_int=32767
ai_levelup_indicator_cooldown=30
ai_levelup_spr=20

function has_value(t,v) -- table,value
 for i,val in ipairs(t) do
  if v==val then
   return true
  end
 end
 return false
end

function make_blinky(pos,txt,off)
	return {pos=pos,txt=txt,off=off}
end

function make_empty_board()
	return {{nil,nil,nil},
      			{nil,nil,nil},
      			{nil,nil,nil}}
end

function make_gs(
	cursor_pos, -- .x and .y
	board,		-- 3x3,nil,"p"or"c"
	diff,		-- 0..255
	timer,		-- round timer (0..255)
	phase,		-- one of phases
	rate,		-- timer rate
	rounds,		-- num.of played rounds
	ptime,		-- time elapsed in current phase (max 32767 frames)
	ptimec,		-- like ptime but cyclic
	note,		-- last note played
	prev_phase, -- previous phase
	blinky,		-- flashy score text
	score,
	dseed,		-- piece drawing rnd seed
	rtime,    	-- time elapsed in current round (max 32767 frames)
	first_round -- first round of the game after main menu
)
 --rudimentary typechecking
 if type(cursor_pos)!="table" then error=1 end
 if type(board)!="table" then error=2 end
 if type(diff)!="number" then error=3 end
 if type(timer)!="number" then error=4 end
 if type(phase)!="string" then error=5 end
 if not has_value(phases,phase) then
 	error=6
 	printh(phase)
 end
 if type(rate)!="number" then error=7 end
 if type(rounds)!="number" then error=8 end
 if type(ptime)!="number" then error=9 end
 if type(ptimec)!="number" then error=9 end
 if type(note)!="number" then error=10 end
 if type(prev_phase)!="string" then error=11 end
 if blinky!=nil and (blinky.pos.x==nil or blinky.pos.y==nil or blinky.txt==nil or blinky.off==nil) then error=12 end
 if type(score)!="number" then error=13 end
 if type(dseed)!="number" then error=14 end
 if type(rtime)!="number" then error=15 end
 if type(first_round)!="boolean" then error=16 end
 --	
	return {
		cursor_pos=cursor_pos,
		board=board,
		diff=diff,
		timer=timer,
		phase=phase,
		rate=rate,
		rounds=rounds,
		ptime=ptime,
		ptimec=ptimec,
		note=note,
		prev_phase=prev_phase,
		blinky=blinky,
		score=score,
		dseed=dseed,
		rtime=rtime,
		first_round=first_round
	}
end

function make_init_gs()
	return 
		make_gs(
			{x=0,y=0},
			make_empty_board(),
			1,
			255,
			"cpu",
			initial_rate,
			0,
			0,
			0,
			0,
			"cpu",
			nil,
			0,
			flr(rnd()*30000),
			0,
			true
		)
end

function make_ms(
	phase,		-- one of m_phases
	ptimec,		-- cyclic phase timer
	ptime,		-- non-cyclic phase timer
	dseed 		-- drawing rnd seed
)
 --rudimentary typechecking
 if type(phase)!="string" then error=128+5 end
 if not has_value(m_phases,phase) then
 	error=128+6
 	printh(phase)
 end
 if type(ptimec)!="number" then error=128+7 end
 if type(ptime)!="number" then error=128+8 end
 if type(dseed)!="number" then error=128+9 end
 --	
	return {
		phase=phase,
		ptimec=ptimec,
		ptime=ptime,
		dseed=dseed
	}
end

function make_init_ms()
 return make_ms("main",0,0,flr(rnd()*30000))
end

function _init()
	gs=make_init_gs()
	ms=make_init_ms()
end

// c-style ternary operator
function t(cond,a,b)
	if cond==0 or cond==nil or
	not cond then
		return b
	else
		return a
	end
end

// c-style not.
function neg(a)
	return t(a,0,1)
end

// clamp (value, max, min)
function cla(x,mn,mx)
 return t(x>mx,mx,t(x<mn,mn,x))
end

// menuitem 1 callback
function set_control(b)
	if (b&1>0) then
		control_rz=0
		menuitem(1,"control:normal➡️",set_control)
	elseif (b&2>1) then
		control_rz=1
		menuitem(1,"control:⬅️selfcnt",set_control)
	end
end

menuitem(
	1,
	"control:normal➡️",
	set_control)			
	
-- user input structure
function make_input()
	return {
		btn_⬅️=btn(⬅️),
		btn_➡️=btn(➡️),
		btn_⬆️=btn(⬆️),
		btn_⬇️=btn(⬇️),
		btn_🅾️=btn(🅾️),
		btn_❎=btn(❎),
		btnp_⬅️=btnp(⬅️),
		btnp_➡️=btnp(➡️),
		btnp_⬆️=btnp(⬆️),
		btnp_⬇️=btnp(⬇️),
		btnp_🅾️=btnp(🅾️),
		btnp_❎=btnp(❎),
	}
end

-- random integer	
function irnd(n)
	return flr(rnd(n))
end

-- is n odd?
function odd(n)
	return ((flr(n)&0x01)==0x01)
end

-- is n multiple of four?
function divisible_by_4(n)
	return ((flr(n)&0x03)==0x02)
end

-->8
-- logic
lines={
 {{x=1,y=1},{x=2,y=1},{x=3,y=1}},
 {{x=1,y=1},{x=2,y=2},{x=3,y=3}},
 {{x=1,y=1},{x=1,y=2},{x=1,y=3}},
 {{x=3,y=1},{x=2,y=2},{x=1,y=3}},
 {{x=3,y=1},{x=3,y=2},{x=3,y=3}},
 {{x=1,y=2},{x=2,y=2},{x=3,y=2}},
 {{x=1,y=3},{x=2,y=3},{x=3,y=3}},
 {{x=2,y=1},{x=2,y=2},{x=2,y=3}},
}
corners={
	{x=1,y=1},{x=3,y=1},
	{x=1,y=3},{x=3,y=3}
}

function keys2cursor_pos(input)
	return {
		x=t(input.btn_⬅️,
		    1,
		    t(input.btn_➡️,3,2)),
		y=t(input.btn_⬆️,
		    1,
		    t(input.btn_⬇️,3,2))
	}
end

function get_new_curpos(cp,input)
	local xd=t(input.btnp_⬅️,
	           -1,
	           t(input.btnp_➡️,1,0))
	local yd=t(input.btnp_⬆️,
	           -1,
	           t(input.btnp_⬇️,1,0))
 return {
  x=cla(cp.x+xd,1,3),
  y=cla(cp.y+yd,1,3)
 }
end	

function board_full(board)
	for i=1,3 do
		for j=1,3 do
			if board[i][j]==nil then
				return false
			end
	 end
	end
	return true
end


function put_mark(board,phase,move)
 if move!=nil then
		board[move.x][move.y]=
			t(phase=="player",player_mark,cpu_mark)
	end
	return board
end	

function get_player_move(cpos,board,input)
	if (input.btnp_🅾️ or input.btnp_❎)
	and (board[cpos.x][cpos.y]==nil)	then
		return cpos
 else
 	return nil				
	end
end

-- 3=won,2=can win on next
function line_score(ln,brd,player)
	local score=0
	local tile=nil
	for p in all(ln) do
		tile=brd[p.x][p.y]
  score+=t(tile==player,1,t(tile==nil,0,-2))
	end
	return score
end

-- find 1st empty tile in line l
function find_empty(board,ln)
	for p in all(ln) do
		if board[p.x][p.y]==nil then
			return p
	 end
	end
	return nil
end

function winning_move(board,player)
	for l in all(lines) do
		if line_score(l,board,player)==2 then
			return find_empty(board,l)
		end
	end
	return nil
end

function check_won(board,player)
	for l in all(lines) do
		if line_score(l,board,player)==3 then
			return l
		end
	end
	return nil
end

function empty_corner(b)
	local empty_corners={}
	for c in all(corners) do
		if b[c.x][c.y]==nil then
			add(empty_corners,c)
	 end
	end
	return t(#empty_corners==0,nil,rnd(empty_corners))
end

-- find random empty pos in board b
function find_rnd_empty(b)
 local empty_spaces={}
 for i=1,3 do
  for j=1,3 do
   if b[i][j]==nil then
			 add(empty_spaces,{x=i,y=j})
	  end 
  end
 end
 return t(#empty_spaces==0,nil,rnd(empty_spaces))
end

-- simplified crowley siegler
-- diff(iculty): o..255
-- prob.of choosing optimal move
-- except winning or blocking
-- (255=always choose optimal)
function get_cpu_move(board,diff)
 local get_right=rnd(255)<=diff
 --printh("---------------")
	local w=winning_move(board,cpu_mark)
	if w!=nil and get_right then
		--printh("winning")
		return w
	else -- block
		local b=winning_move(board,player_mark)
		if(b!=nil and get_right)then
 		--printh("blocking")
			return b
		elseif (board[2][2]==nil
			and get_right) then
			-- center
			--printh("center")
			return {x=2,y=2}
	 else
	  local c=empty_corner(board)
	  if(c!=nil	and get_right) then
	   --printh("corner")
	  	return c
	  else 
	   --printh("other")
    return find_rnd_empty(board)
			end
		end
	end
	return nil
end

function next_phase(phase,moved,timer,new_board,ptime,btn_pressed,cpu_won,player_won)
 if phase=="player" then
  if player_won then 
   return "won"
  elseif timer<=0 then
  	return "lost"
  elseif board_full(new_board) then
   return "tie_player"
  else
		 return t(moved,"cpu","player")
  end		
	elseif phase=="cpu" then
	 if cpu_won then
	  return "lost"
	 elseif board_full(new_board) then
	  return "tie_cpu"
		else
			return t(moved,"player","cpu")
  end	
	elseif phase=="lost" then
		return "lost"
	elseif phase=="won" then
		return t(
		 ptime>won_cooldown,"cpu",phase)
	elseif phase=="tie_cpu" then
	 return t(ptime>tie_cooldown,"player",phase)
	elseif phase=="tie_player" then
	 return t(ptime>tie_cooldown,"cpu",phase)
	else
		return phase										 
	end
end

function next_timer(timer,move,rate,phase)
 if phase=="lost" then
  return timer
 else
		return t(move==nil,
	          t((timer-rate>=0),
	            timer-rate,
	            0),
	          max_timer)
	end
end

function next_rate(rate,round_restart,game_start)
	return t(game_start,initial_rate,
		t(round_restart,rate*rate_coeff,rate))
end

function next_ptime(ptime,rst,is_cyclic)
	return t(rst,
	         0,
	         t(ptime<max_int,
	           ptime+1,
	           t(is_cyclic,
	             0,
	             ptime)))
end

function next_diff(diff,round_restart)
 n_diff=diff+diff_inc
	if round_restart then
	 return(t(n_diff>255,255,n_diff))
 else
  return diff
 end
end

function next_note(note,move)
	if move!=nil then
		return t(note==num_notes-1,0,note+1)
 else
 	return note
 end
end

function next_blinky(b)
	if b==nil then
		return nil
	elseif b.off>max_blinky_off then
 	return nil
 else
 	return make_blinky(
		b.pos,
		b.txt,
		b.off+blinky_off_speed)
	end
end

function calc_score_inc(timer)
 local s_bracket=timer/max_timer
 return flr(s_bracket*timer)
end

function next_dseed(dseed,change)
 return t(change,
          dseed+1,
          dseed)
end

function get_level(score)
--	base_score_per_level
	
end

function update_gs(gs,input)
	local n_cpos=
		t(control_rz,
		  keys2cursor_pos(input),
		  get_new_curpos(gs.cursor_pos,input))
	if gs.phase=="player" then
		current_move=get_player_move(gs.cursor_pos,gs.board,input)
	elseif (gs.phase=="cpu" and (gs.timer<240 or gs.ptime>60)) then -- a little delay
		current_move=get_cpu_move(gs.board,gs.diff)
	else
		current_move=nil
	end
	new_board=put_mark(gs.board,gs.phase,current_move)
	player_win_line=check_won(new_board,player_mark)
	cpu_win_line=check_won(new_board,cpu_mark)
	new_phase=next_phase(
		gs.phase,current_move!=nil,
		gs.timer,new_board,gs.ptime,
		input.btnp_🅾️ or input.btnp_❎,
		cpu_win_line!=nil,player_win_line!=nil)
	local game_start=(new_phase!=gs.phase) and (gs.phase=="menu" or gs.phase=="lost");
	local round_restart=
		(new_phase!=gs.phase) and
		(gs.phase=="won" or
		 gs.phase=="lost" or
		 gs.phase=="tie_player" or
		 gs.phase=="tie_cpu");
	local score_inc=calc_score_inc(gs.timer)
	local new_score=
		t(gs.phase=="player" and current_move!=nil,
		  gs.score+score_inc,
		  gs.score)
	local new_blinky=
		t(gs.phase=="player" and current_move!=nil,
		  make_blinky(current_move,tostr(score_inc),0),
		  nil)
	return make_gs(
		n_cpos,
		t(round_restart,make_empty_board(),new_board),
		next_diff(gs.diff,round_restart),
		next_timer(gs.timer,current_move,gs.rate,gs.phase),
		new_phase,
		next_rate(gs.rate,round_restart,game_start),
		gs.rounds+t(round_restart,1,0),
		next_ptime(gs.ptime,gs.phase!=new_phase,false),
		next_ptime(gs.ptimec,gs.phase!=new_phase,true),
		next_note(gs.note,current_move),
		gs.phase,
		t(new_blinky==nil,
		  t(gs.blinky==nil,
		    nil,
		    next_blinky(gs.blinky)),
		  new_blinky),
		new_score,
		t(gs.ptimec&0x02==0x02,
		  gs.dseed+1,
		  gs.dseed),
		t(round_restart,
		  0,
		  t(gs.rtime<max_int,gs.rtime+1,gs.rtime)),
		t(gs.phase=="won" or gs.phase=="lost" or
		  gs.phase=="tie_player" or gs.phase=="tie_cpu",
		  false,
		  gs.first_round)
	)
end

function next_m_phase(phase,btnpressed,return_to_main)
 if phase=="main" then
  return t(btnpressed,"playing","main")
 elseif phase=="playing" then
  return t(return_to_main,"main","playing")
 else -- should never get here
  return make_init_ms()
 end
end

function update_ms(ms,gs,input)
 local btnpressed=
  input.btnp_❎ or input.btnp_🅾️
 local next_phase=next_m_phase(
	 ms.phase,
	 btnpressed,
	 gs.phase=="lost"
	 	and gs.ptime>lost_cooldown
	 	and (input.btnp_❎ or input.btnp_🅾️))
	return make_ms(
		next_phase,
		next_ptime(ms.ptimec,false,true),
		next_ptime(ms.ptime,false,false),
		t(ms.ptimec&0x02==0x02,
		  ms.dseed+1,
		  ms.dseed))
end

function _update()
 input=make_input()
 ms=update_ms(ms,gs,input)
 if ms.phase=="main"
 and (input.btnp_❎ or input.btnp_🅾️) then
 	gs=make_init_gs() 
 elseif ms.phase=="playing" then
		gs=update_gs(gs,input)
		fire_sfx(gs,gs.prev_phase)
	end
end
-->8
-- drawing
title_phases={["slow_blink"]=1, ["faster_blink"]=2, ["moving_up"]=3, ["full_title"]=4}
title_phase_start_time={["slow_blink"]=0, ["faster_blink"]=60, ["moving_up"]=90, ["full_title"]=105}
title_phase_duration={["slow_blink"]=60, ["faster_blink"]=30, ["moving_up"]=15, ["full_title"]=0}	--	full_title duration is unlimited

function rect2(x,y,w,h,c)
	rect(x,y,x+w,y+h,c)
end

function rectfill2(x,y,w,h,c)
	rectfill(x,y,x+w,y+h,c)
end

-- vector figure table format: f[stroke][point in stroke][coord (x or y) of point]

function make_circle(npts,r)
	local f={}
	local stroke={}
	for p=1,npts-1 do
		add(stroke,{r*cos((p-1)/(npts-1)),r*sin((p-1)/(npts-1))})
	end
	add(stroke,{r,0})
	add(f,stroke)
 	return f
end

f_circle=make_circle(12,tile_side*0.35)
f_ecks={
	{{0,0},{23,23}},
	{{23,0},{0,23}}
}
f_title_line1={
	{{2,8},{32,5}},		-- T
	{{13,7},{13,26}},
	{{25,14},{25,25}},	-- I
	{{43,4},{33,11},{34,20},{44,25}}, -- C
	{{45,8},{65,3}},	-- T
	{{52,6},{52,22}},
	{{58,23},{63,11},{70,34}},	-- A
	{{79,8},{71,11},{72,20},{84,20}}, -- C
	{{74,2},{98,1}},	-- T
	{{86,2},{86,14}},
	{{92,9},{102,9},{101,18},{92,17},{92,9}},	-- O
	{{121,1},{107,4},{109,20},{121,20}},	--	E
	{{108,11},{120,11}}
}
f_title_line2={
	{{3,39},{4,6},{23,17},{9,26},{27,35}},	--	r
	{{33,17},{34,27},{40,33},{48,25},{45,14}}, -- u
	{{62,3},{54,26},{72,23},{57,44}},	-- s
	{{70,10},{77,36}},	--	h
	{{86,8},{88,32}},
	{{70,23},{96,19}}
}

-- brush line
function bline(x0,y0,x1,y1,dd,brush)
	-- dd = brush diameter
	-- brush = brush sprite number
	local dx=x1-x0
	local dy=y1-y0
	local d=sqrt(dx*dx+dy*dy)
	local ns=flr(d/dd) -- num of segments
	local dxs=dx/ns
	local dys=dy/ns
	local x=x0
	local y=y0
	for seg=1,ns do
		spr(brush,x,y)
		x+=dxs
		y+=dys
	end
end

-- draw figure table
-- blink chance: probability (0.0..1.0) that a certain stroke
-- will not be drawn (used for special effects)
function draw_figure(f,xoff,yoff,brush,blink_chance)
	-- previous random offset x/y
	local prox,proy=irnd(2),irnd(2)
	local rox,roy
	for stroke in all(f) do -- p=1,f.npts-1 do
		if rnd(1)>blink_chance then
			n_pts=count(stroke)
			for p=1,n_pts-1 do
				rox=irnd(2)
				roy=irnd(2)
				bline(
					stroke[p][1]+xoff+prox,
					stroke[p][2]+yoff+proy,
					stroke[p+1][1]+xoff+rox,
					stroke[p+1][2]+yoff+roy,
					1,
					brush)
				prox=rox
				proy=roy
			end
		end
	end
end

function draw_board()
	for i=1,3 do
		for j=1,3 do
				rect2(
					tile_off_x+tile_side*(j-1),
					tile_off_y+tile_side*(i-1),
					tile_side,tile_side,8)
				rect2(
					tile_off_x+tile_side*(j-1)+1,
					tile_off_y+tile_side*(i-1)+1,
					tile_side-2,tile_side-2,8)	
		end 
	end
end

function draw_cursor(cpos,phase,ptime)
 local color_=
 	t(phase=="cpu",
 	  cursor_dis_color,
 	  t(odd(ptime),
 	    cursor_a_color,
 	    cursor_b_color))
 rect2(
		tile_off_x+(cpos.x-1)*tile_side+cursor_off,
		tile_off_y+(cpos.y-1)*tile_side+cursor_off,
		cursor_side,cursor_side,
		color_)
end

-- i,j = position in board
function draw_piece(piece,i,j,dseed,highlight)
	local figure=t(piece=="❎",f_ecks,f_circle)
	local additional_offset=t(piece=="❎", -1, -.6)
	local x0=tile_off_x+(i+additional_offset)*tile_side+1
	local y0=tile_off_y+(j+additional_offset)*tile_side+1
	local backup_seed=rnd()
	if piece_shadows and not highlight then
		srand(dseed)
		draw_figure(figure,x0+1,y0+1,shadow_brush,0)
	end
	srand(dseed)
	draw_figure(figure,x0,y0,t(highlight,hi_brush,t(piece=="❎",x_brush,o_brush)),0)
	srand(backup_seed)
end

function draw_pieces(board,dseed,win_line,ptimer)
	for i=1,3 do
		for j=1,3 do
			local piece=board[i][j]
			local highlight=false
			if win_line!=nil then
				for p in all(win_line) do
					if p.x==i and p.y==j and (ptimer&0x01==0x01) and (ptimer<60) then
						highlight=true
					end
				end
			end
			if piece!=nil then
				draw_piece(piece,i,j,dseed+i*j,highlight)
			end
		end
	end
end

function draw_timer(timer)
	rect2(
		timer_off_x,
		timer_off_y,
		tile_side*3,
		timer_height,
		8)
	rectfill2(
	 timer_off_x+1,
	 timer_off_y+1,
	 ((tile_side*3-2)*timer)/255,
	 timer_height-2,
	 14)
	local text_x=screen_side/2-12 -- 12=text width/2
	if timer > max_timer/4 or odd(timer) or timer==0 then	-- blink when timer low
		sspr(0,8,24,8,text_x,timer_off_y+2)
	end
end

function draw_usr_msg(phase,rounds,ptime)
	if phase=="lost" then
		print("lost",screen_side/4,
			screen_side/4,7)
	 print("rounds played: "..rounds,
	  screen_side/4,
			screen_side/4+10,7)
		if ptime>lost_cooldown then
			print("press 🅾️/❎ to restart",
				screen_side/4,
				screen_side/4+20,7)
		end
	elseif phase=="won" then
	 print("won",screen_side/4,
			screen_side/4,7)
	elseif phase=="tie_cpu"
	or phase=="tie_player" then
		print("tie",screen_side/4,
			screen_side/4,7)
 end
end

function draw_info(diff,rate,score)
	print("diff="..diff.."/255 rate="..rate,0,0,7)
	print("score="..score,0,123,7)
end

function draw_blinky(b,blink_phase)
 	if blink_phase==0 then
		print("\^w"..b.txt,
			  (b.pos.x-1)*tile_side+tile_off_x+(tile_side-24)/2,
			  b.pos.y*tile_side-b.off-tile_side/2+1,
			  1)
		print("\^w"..b.txt,
	       	  (b.pos.x-1)*tile_side+tile_off_x+(tile_side-24)/2,
	          b.pos.y*tile_side-b.off-tile_side/2,
	          10)
	end
end

function draw_ai_level(d)
 rect2(
		r_sidebar_x,
		sidebar_y,
		sidebar_w,
		sidebar_h,
		8)
	local ai_bar_len=d/255*(sidebar_h-2)
	rectfill2(
	 r_sidebar_x+1,
	 sidebar_y+sidebar_h-1-ai_bar_len,
	 sidebar_w-2,
	 ai_bar_len,
	 14)	
	sspr(64,0,8,48,r_sidebar_x+2,
	 sidebar_y+sidebar_h-48-1)
end

-- integer to chars, zero-padded
function draw_int_to_chars(i,digits) -- integer
 local iter=0 -- iterations
	while iter<digits	do
	 local d=i%10 -- digit
		i-=d
		i/=10
		iter+=1
		spr(digit_sprite_base+d,
					 score_lsd_x,
					 score_lsd_y+iter*7)
	end
end

function draw_score(score)
 local score_text_h=31
	sspr(0,24,8,32,
	     score_lsd_x,
	     sidebar_y+sidebar_h-score_text_h-1)
	draw_int_to_chars(score,score_digits)
end

function get_title_phase(ptime)
	if ms.ptime < title_phase_start_time["faster_blink"] then
		return "slow_blink"
	elseif ms.ptime < title_phase_start_time["moving_up"] then
		return "faster_blink"
	elseif ms.ptime < title_phase_start_time["full_title"] then
		return "moving_up"
	else
		return "full_title"
	end
end

function get_title_y_off(title_phase,ptime)
	assert(title_phases[title_phase]!=nil)
	local title_max_y_off=20
	if title_phase=="slow_blink" or title_phase=="faster_blink" then
		return 0
	elseif title_phase=="moving_up" then
		return title_max_y_off*(ptime-title_phase_start_time["moving_up"])/title_phase_duration["moving_up"]
	else
		return title_max_y_off
	end
end

function get_title_blink_chance(title_phase,ptime)
	assert(title_phases[title_phase]!=nil)
	local slow_blink_rate=0.95
	if title_phase=="slow_blink" then
		return slow_blink_rate
	elseif title_phase=="faster_blink" then
		return slow_blink_rate*(1-(ptime-title_phase_start_time["faster_blink"])/title_phase_duration["faster_blink"])
	else
		return 0
	end
end

function draw_menu(title_phase,ptime,ptimec,dseed)
	assert(title_phases[title_phase]!=nil)
	local backup_seed=rnd()
	srand(dseed)
	local title_y_off=get_title_y_off(title_phase,ptime)
	local title_blink_chance=get_title_blink_chance(title_phase,ptime)
	draw_figure(f_title_line1,
				-2,25-title_y_off,
				title_brush_1,
				title_blink_chance)
	draw_figure(f_title_line2,
				19,50-title_y_off,
				title_brush_2,
				title_blink_chance)
	srand(backup_seed)
	if title_phase=="full_title" then
		if (ptimec&0x08==0x08) then
			print("press 🅾️/❎ to start",25,85,14)
		end
		print("hang in against the cpu",20,105,14)
		 print("for as long as possible",20,115,14)
	end
end

function draw_hilite(lin)
 if lin!=nil then
  local x0=(lin[1].x-0.5)*tile_side+tile_off_x
  local y0=(lin[1].y-0.5)*tile_side+tile_off_y
  local x1=(lin[3].x-0.5)*tile_side+tile_off_x
  local y1=(lin[3].y-0.5)*tile_side+tile_off_y
	 line(x0,y0,x1,y1,7)
 end
end

function draw_ai_levelup_indicator(rtime, first_round)
	if rtime < ai_levelup_indicator_cooldown
	   and divisible_by_4(rtime)
	   and not first_round then
		spr(ai_levelup_spr, r_sidebar_x+2, sidebar_y+sidebar_h/8, 1, 2)
	end
end

function _draw()
	cls(1)
	if ms.phase=="main" then
		local tp = get_title_phase(ms.ptime)
		printh(""..tp.."@"..ms.ptime)
		draw_menu(tp,ms.ptime,ms.ptimec,ms.dseed)
	elseif ms.phase=="playing" then
		draw_board()
		if gs.phase=="cpu"
		or gs.phase=="player" then
			draw_cursor(gs.cursor_pos,gs.phase,gs.ptimec)
		end
		local win_line=t(player_win_line==nil,cpu_win_line,player_win_line)
		draw_pieces(gs.board,gs.dseed,win_line,gs.ptime)
		if gs.blinky!=nil then
			draw_blinky(gs.blinky,0) --gs.ptimec&0x01)
		end
		draw_timer(gs.timer)
		draw_ai_levelup_indicator(gs.rtime, gs.first_round)
		draw_ai_level(gs.diff)
		draw_score(gs.score)
		draw_usr_msg(gs.phase,gs.rounds,gs.ptimec)
		--draw_info(gs.diff,gs.rate,gs.score)
	end
	if error>0 then
		printh("error="..error)
	end
end
-->8
-- sfx
num_notes=8
move_ding_ch=0
move_sfx_num=2 -- 0 and 1
ding_len=8

--	sfx n [ch [offset [length]]]
function fire_sfx(gs,old_phase)
	local note=t(gs.note==0,7,gs.note-1)
	local tile_set=
		(gs.phase=="cpu" and old_phase=="player")
		or	(gs.phase=="player" and old_phase=="cpu")
		or (gs.phase=="won" and old_phase=="player")  		
  or (gs.phase=="lost" and old_phase=="cpu")
  or ((gs.phase=="tie_cpu" or gs.phase=="tie_player")
      and gs.ptime==0)
 if tile_set then
 	sfx(t(note<4,0,1),
 	    move_ding_ch,
 	    (note*8)%32,
 	    ding_len)
 end
end
__gfx__
0000000099999999bbbbbbbb00000000000000000000000000000000000000000000000700000007009ff7000000000000000000000000000000000000000000
0000000099999999bbbbbbbb00000000000000000000000000000000003bb700000000070000000709f777700000000000000000000000000000000000000000
0070070099999999bbbbbbbb00099000000bb000000550000007700003b7777000000007000000079f7777770000000000000000000000000000000000000000
0007700099999999bbbbbbbb0099990000bbbb0000555500007777000b7777b00000000700000007f777777f0000000000000000000000000000000000000000
0007700099999999bbbbbbbb0099990000bbbb0000555500007777000b7777b00777777707777777f777777f0000000000000000000000000000000000000000
0070070099999999bbbbbbbb00099000000bb000000550000007700003b77b3077777777777777779f7777f90000000000000000000000000000000000000000
0000000099999999bbbbbbbb00000000000000000000000000000000003bb300000000000000000009f77f900000000000000000000000000000000000000000
0000000099999999bbbbbbbb00000000000000000000000000000000000000000700700707007007009ff9000000000000000000000000000000000000000000
00000000000000000000000000000000000700000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
77777770770770007707777770000000007670000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
55775550770770007707755550000000077767000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
00770000770777077707777770000000777776700000000000000000000000000777777707777777000000000000000000000000000000000000000000000000
00770000770777077707755550000000557775500000000000000000000000007777777777777777000000000000000000000000000000000000000000000000
00770000770775757707700000000000007770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00770000770770707707777770000000005550000000000000000000000000000700000007000000000000000000000000000000000000000000000000000000
00550000550550505505555550000000700707600000000000000000000000000070000000700000000000000000000000000000000000000000000000000000
00000000000000000000000000000000700707060000000000000000000000000007000000070000000000000000000000000000000000000000000000000000
00000000000000000000000000000000700707070000000000000000000000000000770000007700000000000000000000000000000000000000000000000000
00000000000000000000000000000000700707750000000000000000000000000000007700000077000000000000000000000000000000000000000000000000
00000000000000000000000000000000700707500000000000000000000000000007777700077777000000000000000000000000000000000000000000000000
00000000000000000000000000000000577507000000000000000000000000000777770007777700000000000000000000000000000000000000000000000000
00000000000000000000000000000000055005000000000000000000000000000770000007700000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
70007007000000000000000000000000000000000000000000000000000000000700700707007007000000000000000000000000000000000000000000000000
70007007000000000000000000000000000000000000000000000000000000000777777707777777000000000000000000000000000000000000000000000000
70007007000000000000000000000000000000000000000000000000000000007777777777777777000000000000000000000000000000000000000000000000
70007007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000000000700000007000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000000000700000007000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000700000007000000000000000000000000000000000000000000000000
07770007000000000000000000000000000000000000000000000000000000000000000700000007000000000000000000000000000000000000000000000000
77777070000000000000000000000000000000000000000000000000000000000777777707777777000000000000000000000000000000000000000000000000
70007070000000000000000000000000000000000000000000000000000000007777777777777777000000000000000000000000000000000000000000000000
70007700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000777777700000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000007777777700000000000000000000000000000000000000000000000000000000
07777770000000000000000000000000000000000000000000000000000000000000007700000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000000770000000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000007077700000000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000007770700000000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000777700000000000000000000000000000000000000000000000000000000000
07777770000000000000000000000000000000000000000000000000000000000007777000000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000000000077700000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07777770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
70007770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
70077777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77770007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07700007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07777770777777770777000707770770777777777007777007007770777700000770777007777700000000000000000000000000000000000000000000000000
77777777777777777777700777777777777777777077777770077777777700007777777777777770000000000000000000000000000000000000000000000000
70000007070000007000700777007077700070007077007770070007700070007007000770007007000000000000000000000000000000000000000000000000
70000007007000007000070770007007070070007070000770070007700007007007000770007007000000000000000000000000000000000000000000000000
77777777000700007700007770007007007070007070000707777777700000707777777777777007000000000000000000000000000000000000000000000000
07777770000070000700000707000070000770007770000700777770700000070770777007770070000000000000000000000000000000000000000000000000
__label__
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888eeeeee888777777888888888888888888888888888888888888888888888888888888888888888ff8ff8888228822888222822888888822888888228888
8888ee888ee88778877788888888888888888888888888888888888888888888888888888888888888ff888ff888222222888222822888882282888888222888
888eee8e8ee87777877788888e88888888888888888888888888888888888888888888888888888888ff888ff888282282888222888888228882888888288888
888eee8e8ee8777787778888eee8888888888888888888888888888888888888888888888888888888ff888ff888222222888888222888228882888822288888
888eee8e8ee87777877788888e88888888888888888888888888888888888888888888888888888888ff888ff888822228888228222888882282888222288888
888eee888ee877788877888888888888888888888888888888888888888888888888888888888888888ff8ff8888828828888228222888888822888222888888
888eeeeeeee877777777888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
11111171111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111177111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111171111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111771111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1eee1ee11ee111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1e111e1e1e1e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1ee11e1e1e1e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1e111e1e1e1e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1eee1e1e1eee11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1eee1e1e1ee111ee1eee1eee11ee1ee1111116611666161616661111166611661666166616611171166611661666166616611111166616161666166111111666
1e111e1e1e1e1e1111e111e11e1e1e1e111116161611161611611111161616161616161616161711161616161616161616161111116116161616161611111666
1ee11e1e1e1e1e1111e111e11e1e1e1e111116161661116111611111166116161666166116161711166116161666166116161111116116161661161611111616
1e111e1e1e1e1e1111e111e11e1e1e1e111116161611161611611111161616161616161616161711161616161616161616161171116116161616161611711616
1e1111ee1e1e11ee11e11eee1ee11e1e111116161666161611611666166616611616161616661171166616611616161616661711116111661616161617111616
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111eee1eee1eee1e1e1eee1ee11111166611661666166616611111111111111111111111111111111111111111111111111111111111111111111111111111
11111e1e1e1111e11e1e1e1e1e1e1111161616161616161616161111111111111111111111111111111111111111111111111111111111111111111111111111
11111ee11ee111e11e1e1ee11e1e1111166116161666166116161111111111111111111111111111111111111111111111111111111111111111111111111111
11111e1e1e1111e11e1e1e1e1e1e1111161616161616161616161111111111111111111111111111111111111111111111111111111111111111111111111111
11111e1e1eee11e111ee1e1e1e1e1111166616611616161616661111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1eee1ee11ee111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1e111e1e1e1e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1ee11e1e1e1e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1e111e1e1e1e11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1eee1e1e1eee11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11661666166611111666111116661166161616661111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16111611116111111616111116661616161616111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16111661116111111666111116161616161616611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16161611116111111611111116161616166616111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16661666116116661611166616161661116116661111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16611666161616661111166616161666166111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16161611161611611111116116161616161611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16161661116111611111116116161661161611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16161611161611611111116116161616161611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
16161666161611611666116111661616161611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1eee1e1e1ee111ee1eee1eee11ee1ee1111116161666166116661666166611111166116611711166116611711111111111111111111111111111111111111111
1e111e1e1e1e1e1111e111e11e1e1e1e111116161616161616161161161111111611161117111611161111171111111111111111111111111111111111111111
1ee11e1e1e1e1e1111e111e11e1e1e1e111116161666161616661161166111111611166617111611166611171111111111111111111111111111111111111111
1e111e1e1e1e1e1111e111e11e1e1e1e111116161611161616161161161111111616111617111616111611171111111111111111111111111111111111111111
1e1111ee1e1e11ee11e11eee1ee11e1e111111661611166616161161166616661666166111711666166111711111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111e1111ee11ee1eee1e1111111166166611661166111116161666161611661666116616161666116611661666111116661166116611711171111111111111
11111e111e1e1e111e1e1e1111111611161616161611177716161611161616111116161116161616161116161616111116161616161117111117111111111111
11111e111e1e1e111eee1e1111111611166616161666111116611661166616661666161116161661166616161661111116661616166617111117111111111111
11111e111e1e1e111e1e1e1111111611161116161116177716161611111611161611161116161616111616161616111116111616111617111117111111111111
11111eee1ee111ee1e1e1eee11111166161116611661111116161666166616611666116611661616166116611616166616111661166111711171111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111e1111ee11ee1eee1e1111111666166611661616166611111111111111111111111111111111111111111111111111111111111111111111111111111111
11111e111e1e1e111e1e1e1111111616166616161616161117771111111111111111111111111111111111111111111111111111111111111111111111111111
11111e111e1e1e111eee1e1111111666161616161616166111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111e111e1e1e111e1e1e1111111611161616161666161117771111111111111111111111111111111111111111111111111111111111111111111111111111
11111eee1ee111ee1e1e1eee11111611161616611161166611111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
111111111666117111661166111116661616166616611111116616661666111116661111166611661616166611711166166611661166117111111cc11ccc1c11
111111111161171116111611111111611616161616161111161116111161111116161111166616161616161117111611161616161611111711111c1c11c11c11
111111111161171116111666111111611616166116161111161116611161111116661111161616161616166117111611166616161666111711111c1c11c11c11
111111111161171116161116111111611616161616161171161616111161111116111111161616161666161117111611161116161116111711711c1c11c11c11
111111111161117116661661117111611166161616161711166616661161166616111666161616611161166611711166161116611661117117111c1c1ccc1ccc
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111eee1eee1eee1e1e1eee1ee11111166616661616166611111166116611711111111111111111111111111111111111111111111111111111111111111111
11111e1e1e1111e11e1e1e1e1e1e1111166616161616161111111611161117111111111111111111111111111111111111111111111111111111111111111111
11111ee11ee111e11e1e1ee11e1e1111161616661661166111111611166617111111111111111111111111111111111111111111111111111111111111111111
11111e1e1e1111e11e1e1e1e1e1e1111161616161616161111111616111617111111111111111111111111111111111111111111111111111111111111111111
11111e1e1eee11e111ee1e1e1e1e1111161616161616166616661666166111711111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111116616661166116611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111161116161616161111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111161116661616166611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111161116111616111611711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111116616111661166117111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111166616161666166111111171116611661111166616161666166111111666166611661616166611711111111111111111111111111111111111111111
11111111116116161616161617771711161116111111116116161616161611111616166616161616161111171111111111111111111111111111111111111111
11111111116116161661161611111711161116661111116116161661161611111666161616161616166111171111111111111111111111111111111111111111
11111111116116161616161617771711161611161111116116161616161611711611161616161666161111171171111111111111111111111111111111111111
11111111116111661616161611111171166616611171116111661616161617111611161616611161166611711711111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111166611661666166616611111166116661616166611111666116616661666166111711111111111111111111111111111111111111111111111111111
11111111161616161616161616161777161616111616116111111616161616161616161617111111111111111111111111111111111111111111111111111111
11111111166116161666166116161111161616611161116111111661161616661661161617111111111111111111111111111111111111111111111111111111
11111111161616161616161616161777161616111616116111111616161616161616161617111111111111111111111111111111111111111111111111111111
11111111166616611616161616661111161616661616116116661666166116161616166611711111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
82888222822882228888822882828882828882228888888888888888888888888888888888888888888888888222822282228882822282288222822288866688
82888828828282888888882882828828828882888888888888888888888888888888888888888888888888888882828288828828828288288282888288888888
82888828828282288888882882228828822282228888888888888888888888888888888888888888888888888222822288228828822288288222822288822288
82888828828282888888882888828828828288828888888888888888888888888888888888888888888888888288888288828828828288288882828888888888
82228222828282228888822288828288822282228888888888888888888888888888888888888888888888888222888282228288822282228882822288822288
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888

__map__
0101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
3104000013450000000000000000000000000000000000001a4500000000000000000000000000000000000018450000000000000000000000000000000000002345000000000000000000000000000000000000
3104000015450000000000000000000000000000000000001a4500000000000000000000000000000000000018450000000000000000000000000000000000002345000000000000000000000000000000000000
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344
00 40424344

