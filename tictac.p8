pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
-- state def./constants
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
title_phase=nil
-- constants
tile_side=33
screen_side=128
screen_h=128
screen_w=128
screen_h_half=screen_h/2
screen_w_half=screen_w/2
tile_off_x=(screen_w-tile_side*3)/2
tile_off_y=(screen_h-tile_side*3)/4
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
	"tie_cpu","tie_player","won","recap"
}
m_phases={"main","playing"} -- menu phases
lost_vanish_dur=40
lost_cooldown=40
tie_cooldown=20
won_cooldown=20
recap_cooldown=80
error=0
control_rz=0 -- control return to zero
max_blinky_off=tile_side*0.2
blinky_off_speed=0.5
max_timer=255
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
title_brush_1_array={11,27,43,59}	--	from darker to lighter
title_brush_2=42
title_brush_1_shadow=60
title_brush_2_shadow=58
piece_shadows=true
max_int=32767
ai_levelup_indicator_cooldown=30
ai_levelup_spr=20
fade_white_to_dark_blue={7,6,13,1}
title_scroll_message_str="hang in against the cpu for as long as possible 😐😐😐 choose your move before the time runs out ⧗⧗⧗ fight an increasingly tough cpu 🐱🐱🐱 don't give up ∧∧∧ "
title_scroll_message_length=151+3+3+3+3 -- special chars are double-width, we can't use lua's string length
title_scroll_message_speed=2 -- pixels per 1/30 second
title_scroll_message_width=print(title_scroll_message_str,tscrollx,screen_w,screen_h) -- prints off-screen just to calculate printed string length
sparkle_sprite_1=12
sparkle_sprite_2=28
number_of_bgfx=2 -- number of different background effects
max_cpu_diff=230 -- cpu never gets perfect (that'd be 255)

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
	first_round,-- first round of the game after main menu
	last_move   -- last move performed, either by AI or human (or cat, etc.)
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
 if (last_move!=nil and (last_move.x==nil or last_move.y==nil or type(last_move.x)!="number" or type(last_move.y)!="number")) then error=17 end
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
		first_round=first_round,
		last_move=last_move
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
			true,
			nil
		)
end

function make_ms(
	phase,		-- one of m_phases
	ptimec,		-- cyclic phase timer
	ptime,		-- non-cyclic phase timer
	dseed,		-- drawing rnd seed
	tscrollx,	-- title scrolling message x coord
	bgfx		-- background effect
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
	if type(tscrollx)!="number" then error=128+10 end
	if type(bgfx)!="number" then error=128+11 end
	--	
	return {
		phase=phase,
		ptimec=ptimec,
		ptime=ptime,
		dseed=dseed,
		tscrollx=tscrollx,
		bgfx=bgfx
	}
end

function make_init_ms()
 	return make_ms("main",0,0,flr(rnd()*30000),screen_w,0)
end

function _init()
	pal(5, 0x05, 2)
	poke(0x5f5c, 255) -- make btnp never repeat keypresses
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
		board[move.x][move.y]=t(phase=="player",player_mark,cpu_mark)
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

-- simplified from crowley siegler paper
-- diff(iculty): 0..255
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
		return t(btn_pressed and ptime>lost_cooldown,"recap","lost")
	elseif phase=="recap" then
		return "recap"
	elseif phase=="won" then
		return t(ptime>won_cooldown,"cpu",phase)
	elseif phase=="tie_cpu" then
		return t(ptime>tie_cooldown,"player",phase)
	elseif phase=="tie_player" then
	 	return t(ptime>tie_cooldown,"cpu",phase)
	else
		return phase										 
	end
end

function next_timer(timer,move,rate,phase)
 	if phase=="lost" or phase=="recap" then
  		return timer
 	else
		return t(	move==nil,
	          		t((timer-rate>=0),timer-rate,0),
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
	 	return(t(n_diff>max_cpu_diff,max_cpu_diff,n_diff))
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
		 gs.phase=="recap" or
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
		  gs.first_round),
		t(current_move, current_move, gs.last_move),
		gs.bgfx
	)
end

function next_m_phase(phase,ms_ptime,btnpressed,return_to_main)
 if phase=="main" then
  return t(btnpressed and ms_ptime>title_phase_start_times["full_title"],"playing","main")
 elseif phase=="playing" then
  return t(return_to_main,"main","playing")
 else -- should never get here
  return make_init_ms()
 end
end

function next_tscrollx(tscrollx)
	return t(tscrollx<-title_scroll_message_width,
				tscrollx+title_scroll_message_width,
				tscrollx-title_scroll_message_speed)
end

function update_ms(ms,gs,input)
	title_phase = get_title_phase(ms.ptime)
 	local btnpressed=input.btnp_❎ or input.btnp_🅾️
 	local next_phase=next_m_phase(
		ms.phase,
		ms.ptime,
		btnpressed,
		gs.phase=="recap"
			and gs.ptime>recap_cooldown
			and (input.btnp_❎ or input.btnp_🅾️))
	return make_ms(
		next_phase,
		next_ptime(ms.ptimec,false,true),
		next_ptime(ms.ptime,false,false),
		t(ms.ptimec&0x02==0x02,
		  ms.dseed+1,
		  ms.dseed),
		t(title_phase=="full_title",next_tscrollx(ms.tscrollx),ms.tscrollx),
		t(	ms.phase=="playing" and (next_phase!=ms.phase), -- returning to menu
			t(ms.bgfx==(number_of_bgfx-1),0,ms.bgfx+1),
			ms.bgfx))
end

function _update()
 	input=make_input()
 	ms=update_ms(ms,gs,input)
 	if ms.phase=="main" and (input.btnp_❎ or input.btnp_🅾️) then
 		gs=make_init_gs() 
 	elseif ms.phase=="playing" then
		gs=update_gs(gs,input)
		fire_sfx(gs,gs.prev_phase)
	end
end
-->8
-- drawing
title_phases={"wait", "hilite", "ttt_fade", "rush_in", "moving_up",
	"full_title"}
title_phase_durations={
	["wait"]=30,
	["hilite"]=23,
	["ttt_fade"]=15,
	["rush_in"]=30,
	["moving_up"]=15,
	["full_title"]=-1 -- infinite duration
}
default_ptn=0
checkerboard_ptn=0b1010010110100101
apply_to_sprites_ptn=0b.01
transparent_ptn=0b.1
bayer_4x4_fillp_lut={[0]=0b0000000000000000.010,0b1000000000000000.010,0b1000000000100000.010,0b1010000000100000.010,0b1010000010100000.010,0b1010010010100000.010,0b1010010010100001.010,0b1010010110100001.010,0b1010010110100101.010,0b1110010110100101.010,0b1110010110110101.010,0b1111010110110101.010,0b1111010111110101.010,0b1111110111110101.010,0b1111110111110111.010,0b1111111111110111.010,0b1111111111111111.010}

function title_phase_valid(tp)
	for phase in all(title_phases) do
		if phase==tp then
			return true
		end
	end
	return false
end

function calc_title_phase_start_time()
	local start_times={}
	local phase_start=0
	for i, phase in ipairs(title_phases) do
		start_times[phase]=phase_start
		phase_start+=title_phase_durations[phase]
	end
	return start_times
end
title_phase_start_times=calc_title_phase_start_time()

function get_title_phase(ptime)
	for i, title_phase in ipairs(title_phases) do
		if (ptime >= title_phase_start_times[title_phase]) and
				((title_phase_durations[title_phase]==-1) or -- infinite duration
				 (ptime < (	title_phase_start_times[title_phase]+
							title_phase_durations[title_phase])))
		    then
			return title_phase
		end
	end
	return title_phases[1]
end

function rect2(x,y,w,h,c)
	rect(x,y,x+w,y+h,c)
end

function rectfill2(x,y,w,h,c)
	rectfill(x,y,x+w,y+h,c)
end

-- highlighted print: string,x,y,color,highlight color
function hprint(s,x,y,c,h)
	for i=-1,1 do
		for j=-1,1 do
			print(s,x+i,y+j,h)
		end
	end
	print(s,x,y,c)
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
	{{22,1},{15,8},{8,14},{0,23}}
}
f_title_line1={
	{{3,7},{20,3}},	-- t
	{{12,5},{12,30}},
	{{21,9},{21,28}},	--	i
	{{40,3-4},{31,6-4},{28,19-4},{33,32-4},{41,29-4}}, -- c
	{{43-2,6+1},{59-2,3+1}},	-- t
	{{50-2,5+1},{50-2,29+1}},
	{{54,32},{62,7}},	--	a
	{{62,8},{67,29}},
	{{56,28},{66,24}},
	{{82,2-3},{72,5-3},{69,20-3},{74,30-3},{83,28-3}},	--	c
	{{83-1,7},{99-1,4}},	-- t
	{{90-1,6},{90-1,29}},
	{{96,13},{103,11},{103,28},{96,31},{96,13}},	--	o
	{{110,7},{110,32}},	--	e
	{{110,7},{122,4}},
	{{110,20},{120,17}},
	{{110,32},{122,29}}
}
title_line2={
	-- f_r
	{{{10-14,88-36},{10-14,46-36},{28-14,41-36},{35-14,50-36},{29-14,62-36},{10-14,66-36}},
	{{23-14,63-36},{36-14,82-36}}},
	-- f_u
	{{{39-11,46-36},{39-11,77-36},{45-11,85-36},{58-11,83-36},{62-11,73-36},{62-11,39-36}}},
	-- f_s
	{{{82,3},{60,8},{58,17},{62,31},{78,27},{82,36},{79,47},{57,53}}},
	-- f_h
	{{{86,10},{86,51}},
	{{87,29},{111,24}},
	{{110,3},{110,46}}}
}
title_line2_y=58

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
-- dd: brush diameter
function draw_figure(f,xoff,yoff,brush,blink_chance,shake,dd)
	-- mroff: "max random offset"
	local mroff=t(shake,2,0)
	-- prox/proy: "previous random offset x/y"
	local prox,proy=irnd(mroff),irnd(mroff)
	local rox,roy
	for stroke in all(f) do -- p=1,f.npts-1 do
		if rnd(1)>blink_chance then
			n_pts=count(stroke)
			for p=1,n_pts-1 do
				rox=irnd(mroff)
				roy=irnd(mroff)
				bline(
					stroke[p][1]+xoff+prox,
					stroke[p][2]+yoff+proy,
					stroke[p+1][1]+xoff+rox,
					stroke[p+1][2]+yoff+roy,
					dd,
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
	local x0=tile_off_x+(i+additional_offset)*tile_side+t(piece=="❎",1,0)
	local y0=tile_off_y+(j+additional_offset)*tile_side+t(piece=="❎",1,0)
	local backup_seed=rnd()
	if piece_shadows and not highlight then
		srand(dseed)
		draw_figure(figure,x0+1,y0+1,shadow_brush,0,true,1)
	end
	srand(dseed)
	draw_figure(figure,x0,y0,t(highlight,hi_brush,t(piece=="❎",x_brush,o_brush)),0,true,1)
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
	local text_x=screen_side/2-16 -- 16=text width/2
	if timer > max_timer/4 or odd(timer) or timer==0 then	-- blink when timer low
		sspr(0,8,32,8,text_x,timer_off_y+2)
	end
end

-- inspired by locomotion tv channel
function locoprint(str,x,y,depth,colors)
	if colors.border==nil or colors.color_1==nil or colors.color_2==nil then
		printh("error llamada a locoprint")
	end
	big_str="\^t\^w"..str
	-- volume
	for d=1,depth do
		print(big_str,x+depth-1-d,y+depth-1-d,colors.border)
		print(big_str,x+depth-1-d,y+depth-d+1,colors.border)
		print(big_str,x+depth+1-d,y+depth-1-d,colors.border)
	end
	-- front border
	print(big_str,x+depth-1,y+depth-1,colors.border)
	print(big_str,x+depth+1,y+depth-1,colors.border)
	print(big_str,x+depth-1,y+depth+1,colors.border)
	print(big_str,x+depth+1,y+depth+1,colors.border)
	-- front
	print(big_str,x+depth,y+depth,t(band(time(),0b.0001),colors.color_1,colors.color_2))
end

ranks={
	{name="e", 	top=500},
	{name="d", 	top=2000},
	{name="c", 	top=3000},
	{name="b-", top=4000},
	{name="b", 	top=6500},
	{name="b+", top=9000},
	{name="a-", top=11500},
	{name="a", 	top=14000},
	{name="a+", top=20000},
	{name="a++",top=26000},
	{name="s", 	top=28000},
	{name="s+", top=30000},
	{name="s++",top=max_int},
	{name="perfect-max",top=nil}
}
function score2rank(score)
	if score<0 then	--	score var has overflowed
		return ranks[#ranks].name
	else
		for r in all(ranks) do
			if r.top > score then	
				return r.name
			end
		end
	end
end

lost_anim_dur=60
recap_msg_dur=10
recap_msg_depth=3
function draw_usr_msg(phase,rounds,ptime,last_move,score)
	if phase=="lost" then
		local depth=t(	ptime<lost_anim_dur,
						flr(-3.5*sin(ptime/lost_anim_dur/2)),
						0)
		locoprint(	"lost",
					tile_off_x+(last_move.x-1)*tile_side+5,
					tile_off_y+(last_move.y-1)*tile_side+10,
					depth,
					{border=8,color_1=7,color_2=12})
	elseif phase=="recap" then
		local recap_messages={
			{msg="rounds played", start_frame=10, x=13, y=15, color={border=8,color_1=7,color_2=7}},
			{msg=rounds, start_frame=20, x=13, y=31, color={border=8,color_1=7,color_2=12}},
			{msg="score", start_frame=30, x=13, y=47, color={border=8,color_1=7,color_2=7}},
			{msg=t(score<0, "maximum", score), start_frame=40, x=13, y=63, color={border=8,color_1=7,color_2=12}},
			{msg="rank", start_frame=50, x=13, y=79, color={border=8,color_1=7,color_2=7}},
			{msg=score2rank(score), start_frame=60, x=13, y=95, color={border=8,color_1=7,color_2=12}}}
			-- note: when changing recap_messages.start_frame, recalculate global "recap_cooldown" 
		for m in all(recap_messages) do
			if ptime>m.start_frame then
				locoprint(	m.msg, m.x, m.y,
							cla(recap_msg_depth*(ptime-m.start_frame)/recap_msg_dur,0,recap_msg_depth),
							m.color)
			end
		end
		if ptime>recap_cooldown then
			hprint("press 🅾️/❎ to continue", 16, 115, 7, 8)
		end
	elseif phase=="won" then
		local depth=flr(-3.5*sin(ptime/won_cooldown/2))
	 	locoprint(	"won",
		 			tile_off_x+(last_move.x-1)*tile_side+5,
					tile_off_y+(last_move.y-1)*tile_side+10,
					depth,
					{border=8,color_1=7,color_2=12})
	elseif phase=="tie_cpu"	or phase=="tie_player" then
		local depth=flr(-3.5*sin(ptime/tie_cooldown/2))
	 	locoprint(	"tie",
		 			tile_off_x+(last_move.x-1)*tile_side+5,
					tile_off_y+(last_move.y-1)*tile_side+10,
					depth,
					{border=8,color_1=7,color_2=12})
 	end
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
	local ai_bar_len=d/max_cpu_diff*(sidebar_h-2)
	rectfill2(
		r_sidebar_x+1,
		sidebar_y+sidebar_h-1-ai_bar_len,
		sidebar_w-2,
		ai_bar_len,
		14)	
	sspr(120,0,8,64,r_sidebar_x+2,
	 	sidebar_y+sidebar_h-64-1)
end

-- integer to chars, zero-padded
function draw_int_to_chars(score,digits) -- integer
 	local iter=0 -- iterations
	if score<0 then	-- score var has overflowed
		for i=1,digits do
			spr(digit_sprite_base+9,
						score_lsd_x,
						score_lsd_y+i*7)
		end
	else
		while iter<digits do
			local d=score%10 -- digit
			score-=d
			score/=10
			iter+=1
			spr(digit_sprite_base+d,
						score_lsd_x,
						score_lsd_y+iter*7)
		end
	end
end

function draw_score(score)
 	local score_text_h=40
	sspr(0,16,8,40,
	     score_lsd_x,
	     sidebar_y+sidebar_h-score_text_h-1)
	draw_int_to_chars(score,score_digits)
end

function get_title_phase_progress(title_phase,ptime) -- returns [0,1]
	local time_in_this_phase=ptime-title_phase_start_times[title_phase]
	if time_in_this_phase >= title_phase_durations[title_phase] then
		return 1.0
	else
		return time_in_this_phase/title_phase_durations[title_phase]
	end
end

function get_title_y_off(title_phase,ptime)
	assert(title_phase_valid(title_phase))
	local title_max_y_off=20
	if title_phase=="hilite" or title_phase=="ttt_fade" or title_phase=="rush_in" then
		return 0
	elseif title_phase=="moving_up" then
		return title_max_y_off*get_title_phase_progress("moving_up",ptime)
	else
		return title_max_y_off
	end
end

function get_line1_brush(title_phase,ptime)
	assert(title_phase_valid(title_phase))
	number_of_brushes=count(title_brush_1_array)
	if title_phase == "hilite" then
		return title_brush_1_array[number_of_brushes]
	elseif title_phase == "ttt_fade" then
		local faded_brush_index=ceil(get_title_phase_progress(title_phase,ptime)*(number_of_brushes-1))
		--	we must clamp faded_brush_index to 1..number_of_brushes
		return title_brush_1_array[t(faded_brush_index<1,1,ceil(faded_brush_index))]
	else
		return title_brush_1_array[number_of_brushes-1]
	end

end

--function print_scrolling(string,ptime,)

function draw_menu(title_phase,ptime,ptimec,dseed,tscrollx)
	assert(title_phase_valid(title_phase))
	-- setting background color
	local title_y_off=get_title_y_off(title_phase,ptime)
	if title_phase=="full_title" then
		cls(1)
	elseif title_phase=="moving_up" then
		local number_of_fade_colors=count(fade_white_to_dark_blue)
		local current_fade_color=ceil(number_of_fade_colors*get_title_phase_progress("moving_up", ptime))
		cls(fade_white_to_dark_blue[t(current_fade_color>0,current_fade_color,1)])
	else
		cls(0)
	end
	-- draw title line 1
	if title_phase=="hilite" then
		-- moving clipping rectangle only allows to see the "highlighted" portion of the logo
		local starting_x=screen_w*get_title_phase_progress("hilite",ptime)
		clip(starting_x,0,screen_w/title_phase_durations["hilite"]+1,screen_h,false)
	else
		clip()
	end
	if title_phase=="full_title" then	--	shadow
		draw_figure(f_title_line1,
						-2+2,25-title_y_off+2,
						title_brush_1_shadow,
						0, false, 1)
	end
	if title_phase!="wait" then
		draw_figure(f_title_line1,
						-2,25-title_y_off,
						get_line1_brush(title_phase,ptime),
						0, false, 1)
	end
	-- draw title line 2
	if title_phase=="rush_in" or title_phase=="moving_up" or title_phase=="full_title" then
		for i,figure in ipairs(title_line2) do
			local sine_offset=
				t(title_phase=="rush_in",
				  3*sin((i-1)*0.25),
				  3*sin((ptimec&0x1f)/0x1f+(i-1)*0.25))
			local per_letter_offset=0
			local letter_movement_start_progress=(i-1)*.25
			local letter_movement_end_progress=i*.25
			local individual_letter_y_off=0
			if title_phase=="rush_in" then
				local rush_in_progress=get_title_phase_progress("rush_in",ptime)
				if rush_in_progress < letter_movement_start_progress then
					-- out of screen
					individual_letter_y_off = screen_h
				elseif rush_in_progress < letter_movement_end_progress then
					-- moving up
					local sqrt_offset=(letter_movement_end_progress-rush_in_progress)
					individual_letter_y_off = (screen_h)*(sqrt_offset*sqrt_offset)
				else
					-- in its final place
					individual_letter_y_off = 0
				end
			else
				individual_letter_y_off = 0
			end
			if title_phase=="full_title" then
				local trailing_sine_offset=3*sin(((ptimec-4)&0x1f)/0x1f+(i-1)*0.25)
				-- see secondary palette modifications in _init
				fillp(checkerboard_ptn+apply_to_sprites_ptn+transparent_ptn)
				draw_figure(figure,
						8,title_line2_y-title_y_off+trailing_sine_offset+individual_letter_y_off,
						title_brush_2_shadow,
						0, false, 4)
				fillp(default_ptn+apply_to_sprites_ptn+transparent_ptn)
			end
			draw_figure(figure,
						8,title_line2_y-title_y_off+sine_offset+individual_letter_y_off,
						title_brush_2,
						0, false, 2)
		end
	end
	-- sparkling lights
	if title_phase=="full_title" then
		local backup_seed=rnd()
		srand(dseed)
		if rnd()<0.5 then
			spr(t(rnd()<0.75,sparkle_sprite_1,sparkle_sprite_2),
				screen_w*rnd(),screen_h*0.75*rnd(),1,1,rnd()<0.5,false)
		end
		srand(backup_seed)
	end
	-- title screen text
	if title_phase=="full_title" then
		if (ptimec&0x08==0x08) then
			hprint("press 🅾️/❎ to start",25,105,9,0)
		end
		print(title_scroll_message_str,tscrollx,117,11)
		-- print a second copy of the message if the first one
		-- no longer takes up the whole width of the screen
		if tscrollx < -title_scroll_message_width+screen_w then
			print(title_scroll_message_str,tscrollx+title_scroll_message_width,117,11)
		end
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

bgfx_tile_size=8
sw_t=screen_w/bgfx_tile_size
sh_t=screen_h/bgfx_tile_size
fx_r=sw_t -- circle radius
fx_period=8 -- effect period, in seconds
circle_fx_spr=22
function draw_bg_circles(t)
	for tx=0,sw_t/2-1 do
        local px=tx*bgfx_tile_size
        for ty=0,sh_t/2-1 do
            local py=ty*bgfx_tile_size
            local d=sqrt(tx*tx+ty*ty) -- tried putting this in a lut, but no perf gains
            local intensity=17/2*(1.0+sin(d*1/fx_r-t/fx_period))
            fillp(bayer_4x4_fillp_lut[cla(flr(intensity),0,16)])
            spr(circle_fx_spr, px+screen_w_half,py+screen_h_half) -- DR
            spr(circle_fx_spr, px+screen_w_half,-bgfx_tile_size-py+screen_h_half) -- UR
            spr(circle_fx_spr,-bgfx_tile_size-px+screen_w_half,py+screen_h_half) -- DL
            spr(circle_fx_spr,-bgfx_tile_size-px+screen_w_half,-bgfx_tile_size-py+screen_h_half) -- UL
        end
    end
	fillp(0)
end

sep_z=1 	-- distance between grid lines
sep_x=sep_z*50
floor_y=50
field_width=1000
horizon_z=floor_y/8
function draw_bg_grid(t)
	-- cls(0)
	theta=t/32
	for mirror_v in all({-1,1}) do
		-- depth lines
		for i=1,6 do
			local line_z=i*sep_z-(t%sep_z)
			line(64+(cos(theta)*(-field_width)+sin(theta)*floor_y*mirror_v)/line_z,
				64-(-sin(theta)*(-field_width)+cos(theta)*floor_y*mirror_v)/line_z,
				64+(cos(theta)*field_width+sin(theta)*floor_y*mirror_v)/line_z,
				64-(-sin(theta)*field_width+cos(theta)*floor_y*mirror_v)/line_z,
				1)
		end
		-- horizon-parallel lines
		for i=-4,4 do
			local line_x=i*sep_x
			line(64+(cos(theta)*line_x+sin(theta)*floor_y*mirror_v)/horizon_z,
				64-(-sin(theta)*line_x+cos(theta)*floor_y*mirror_v)/horizon_z,
				64+(cos(theta)*line_x+sin(theta)*floor_y*mirror_v)/0.1,
				64-(-sin(theta)*line_x+cos(theta)*floor_y*mirror_v)/0.1,
				1)
		end
		-- horizon
		line(64+(cos(theta)*(-field_width)+sin(theta)*floor_y*mirror_v)/horizon_z,
				64-(-sin(theta)*(-field_width)+cos(theta)*floor_y*mirror_v)/horizon_z,
				64+(cos(theta)*field_width+sin(theta)*floor_y*mirror_v)/horizon_z,
				64-(-sin(theta)*field_width+cos(theta)*floor_y*mirror_v)/horizon_z,
				13)
	end
end

bgfx_table={[0]=draw_bg_grid, draw_bg_circles}
function draw_game(gs)
	cls(0)
	bgfx_table[ms.bgfx](time())
	if gs.phase=="recap" then
		local vanish_progress=gs.ptime/lost_vanish_dur
		fillp(0b.1+bayer_4x4_fillp_lut[cla(flr(16*vanish_progress),0,16)])
	end
	if not (gs.phase=="recap" and gs.ptime>lost_vanish_dur) then
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
	end
	fillp(0)
	draw_usr_msg(gs.phase,gs.rounds,gs.ptimec,gs.last_move,gs.score)
end

function _draw()
	if ms.phase=="main" then
		draw_menu(title_phase,ms.ptime,ms.ptimec,ms.dseed,ms.tscrollx)
	elseif ms.phase=="playing" then
		draw_game(gs)
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
0000000099999999bbbbbbbb00000000000000000000000000000000000000000000000000000000009ff7000000000070000000000000000000000000000000
0000000099999999bbbbbbbb00000000000000000000000000000000003bb700000000070000000009f777700000000007000070000000000000000000000007
0070070099999999bbbbbbbb00099000000bb000000550000007700003b7777000000007000000009f7777770011110000700700000000000000000000000007
0007700099999999bbbbbbbb0099990000bbbb0000555500007777000b7777b00000000700000000f777777f0111111000077000000000000000000000000007
0007700099999999bbbbbbbb0099990000bbbb0000555500007777000b7777b00000000700000000f777777f0111111000077000000000000000000000000007
0070070099999999bbbbbbbb00099000000bb000000550000007700003b77b3000000007000000009f7777f90011110000700700000000000000000000000007
0000000099999999bbbbbbbb00000000000000000000000000000000003bb300777777770000000009f77f900000000007000070000000000000000077777777
0000000099999999bbbbbbbb00000000000000000000000000000000000000007777777700000000009ff9000000000000000007000000000000000077777777
777777707777777077000770777777000007000000133b7000000000000000000000000000000000009999000000000000070000000000000000000000000000
007700000077000077000770770000000076700000133b700011110000bbbb000000000000000000099999900000000000077000000000000000000000000000
007700000077000077707070770000000777670000133b70011111100bbbbbb07000700700000000999999990033330000077000000000000000000070007007
007700000077000077707070770000007777767000133b70011111100bbbbbb07000700700000000999999990333333007777777000000000000000070007007
007700000077000077070070777777005577755000133b70011111100bbbbbb07000700700000000999999990333333077777770000000000000000070007007
007700000077000077070070770000000077700000133b70011111100bbbbbb07000700700000000999999990033330000077000000000000000000070007007
007700000077000077000070770000000055500000133b700011110000bbbb007777777700000000099999900000000000077000000000000000000077777777
007700007777777077000070777777007007076000133b7000000000000000007777777700000000009999000000000000007000000000000000000077777777
00000000000000000000000000000000700707060000000000000000000000000000000000000000009999000000000000000000000000000000000000000000
00000000000000000000000000000000700707070000000000000000000000007700000000000000099999900000000000000000000000000000000077000000
700070070000000000000000000000007007077500000000000000000000000000777700000000000999999000bbbb0000000000000000000000000000777700
70007007000000000000000000000000700707500000000000000000000000000000007700000000099999900bbbbbb000000000000000000000000000000077
70007007000000000000000000000000577507000000000000000000000000000000007700000000099999900bbbbbb000000000000000000000000000000077
700070070000000000000000000000000550050000000000000000000000000000777777000000000999999000bbbb0000000000000000000000000000777777
77777777000000000000000000000000000000000000000000000000000000007777770000000000099999900000000000000000000000000000000077777700
77777777000000000000000000000000000000000000000000000000000000007700000000000000009999000000000000000000000000000000000077000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000005555000000000000000000000000000000000000000000
07770007000000000000000000000000000000000000000000000000000000007000700700000000055555500000000000000000000000000000000070007007
77777007000000000000000000000000000000000000000000000000000000007000700700000000055555500077770000555500000000000000000070007007
70007070000000000000000000000000000000000000000000000000000000007000700700000000055555500777777005555550000000000000000070007007
70007070000000000000000000000000000000000000000000000000000000007000700700000000055555500777777005555550000000000000000070007007
70007700000000000000000000000000000000000000000000000000000000007000700700000000055555500077770000555500000000000000000070007007
77777777000000000000000000000000000000000000000000000000000000007777777700000000055555500000000000000000000000000000000077777777
77777777000000000000000000000000000000000000000000000000000000007777777700000000005555000000000000000000000000000000000077777777
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07777770000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000007
77777777000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000007
70000007000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000007
70000007000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000007
70000007000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000007
77777777000000000000000000000000000000000000000000000000000000007777777700000000000000000000000000000000000000000000000077777777
07777770000000000000000000000000000000000000000000000000000000007777777700000000000000000000000000000000000000000000000077777777
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000007000000700000000000000000000000000000000000000000000000000000000
70000007000000000000000000000000000000000000000000000000000000007000000700000000000000000000000000000000000000000000000000000077
70000007000000000000000000000000000000000000000000000000000000007777777700000000000000000000000000000000000000000000000000000077
70000007000000000000000000000000000000000000000000000000000000007777777700000000000000000000000000000000000000000000000000000000
77777777000000000000000000000000000000000000000000000000000000007000000700000000000000000000000000000000000000000000000070000007
07777770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000070000007
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000070000007
70000770000000000000000000000000000000000000000000000000000000000000007700000000000000000000000000000000000000000000000077777777
70007777000000000000000000000000000000000000000000000000000000000077770000000000000000000000000000000000000000000000000077777777
70007007000000000000000000000000000000000000000000000000000000007700070000000000000000000000000000000000000000000000000070000007
70007007000000000000000000000000000000000000000000000000000000007777070000000000000000000000000000000000000000000000000070000007
77077007000000000000000000000000000000000000000000000000000000000077770000000000000000000000000000000000000000000000000000000000
77777007000000000000000000000000000000000000000000000000000000000000777700000000000000000000000000000000000000000000000000000077
07770007000000000000000000000000000000000000000000000000000000000000007700000000000000000000000000000000000000000000000000000077
00000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07777770777777770777000707770770777777777007777007007770777700000770777007777700000000000000000000000000000000000000000000000077
77777777777777777777700777777777777777777077777770077777777700007777777777777770000000000000000000000000000000000000000000777700
70007007070000007000700777007077700070007077007770070007700070007007000770007007000000000000000000000000000000000000000077000700
70070007007000007000070770007007070070007070000770070007700007007007000770007007000000000000000000000000000000000000000077770700
77777777000700007700007770007007007070007070000707777777700000707777777777777007000000000000000000000000000000000000000000777700
07777770000070000700000707000070000770007770000700777770700000070770777007770070000000000000000000000000000000000000000000007777
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000077
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
01040000134451a4451f4350040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000000
01040000184351f4452444500000000000000000000000002d400254001b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011000000c65500600006000060000600006000060000600006000060000600006000060000600006000060000600006000060000600006000060000600006000060000600006000060000000000000000000000
950400002653500500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
910e00000c4330c4130e5000e5000e57300000000000e5000c3000e5000c4330c4130e5730e50000000000000c4330c41300000000000e5730000000000000000c300000000c4330c4130e5730e5730000000000
050e00000535510300103050030000000000000000000000173001a1000c3551a1050f3551c10011355000000a3551a1001a3001c1001c300101000e300000000000000000000000000000000000000000000000
050e00000535511300103050030000000000000000000000173001a1000c355183000f355133000c355000000f355000000000000000000000000000000000000000000000000000000000000000000000000000
010e0000000000000011325113250c3250f3000f3250f300000000000011325113250c3250f3000f3250f30000000000000f3250f3250a3250f3000d3250d30000000000000f3250f3250a3250f3000d3250d300
011c000011324113201132011320113201132511300113000f3240f3200f3200f3200f3200f325000040000400004000040000400004000040000400004000040000000000000000000000000000000000000000
011c00001132411320113201132011320113250000000000163241632016320163201632016325000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010e00000050500505115550050500505115051155500505005050c5050c555005050f55500505005050050500505005050f5550050500505005050f5550050500505005050a555005050d555005050050500000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010a00000843607436054360443600435004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400000000000000000000000000000000
010a00000045504455074550c45500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010a00000c455074550c4550745500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 08094b44
00 080a4344
00 08090b44
00 080a0b44
00 08090c0b
00 080a0d0b
00 08090e44
00 080a0e0c
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

