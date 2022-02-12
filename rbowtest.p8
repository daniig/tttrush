pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
-- inspired by locomotion tv channel
function locoprint(t,x,y,d)
	-- relieve/depth
	for depth=1,d do
		print(t,x-1-depth,y-1-depth,8)
		print(t,x-1-depth,y-depth+1,8)
		print(t,x+1-depth,y-1-depth,8)
	end
	-- relieve/frontal
	print(t,x-1,y-1,8)
	print(t,x+1,y-1,8)
	print(t,x-1,y+1,8)
	print(t,x+1,y+1,8)
	-- frontal
	local c=0
	if band(time(),0b.0001)!=0 then
		c=7
	else
		c=12
	end
	print(t,x,y,c)
end

function _draw()
	cls(0)
	local t1="\^t\^wwin"
	local t2="\^t\^wlose"
	local t3="\^t\^wtie"
	local x=10
	local y=10
	local d=2*(1+sin(2*time()))
	locoprint(t1,x+d,y+d,d)
	locoprint(t2,x+d,y+30+d,d)
	locoprint(t3,x+d,y+60+d,d)
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
