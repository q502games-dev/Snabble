import { useState, useEffect, useRef, useCallback, useMemo } from "react";

const BOUNTY_POOL = "snake,brain,quest,flame,ghost,magic,sword,brave,crown,shark,eagle,frost,bloom,crisp,dream,feast,grape,honey,jewel,kneel,lunar,mirth,noble,oasis,pearl,reign,solar,torch,vivid,whale".split(",");
const COLS = 24, ROWS = 18, CELL = 28, GW = COLS*CELL, GH = ROWS*CELL;
const VOWELS = new Set("AEIOU"), STICKY_SET = new Set("QZXJ");
const LW = "EEEEEEEEEEEETTTTTTTTTAAAAAAAAOOOOOOOIIIIIIINNNNNNNSSSSSSSHHHHHHRRRRRRLLLLDDDDCCCCUUUMMMMWWFFGGYYPBBVVKKJJXXQZZ";
const SP = {A:1,B:3,C:3,D:2,E:1,F:4,G:2,H:4,I:1,J:8,K:5,L:1,M:3,N:1,O:1,P:3,Q:10,R:1,S:1,T:1,U:1,V:4,W:4,X:8,Y:4,Z:10};
const DIR = {UP:{x:0,y:-1},DOWN:{x:0,y:1},LEFT:{x:-1,y:0},RIGHT:{x:1,y:0}};
const DIRS_ARR = [DIR.UP,DIR.DOWN,DIR.LEFT,DIR.RIGHT];
const rL = ()=>LW[Math.floor(Math.random()*LW.length)];
const rP = (avoid)=>{let p,t=0;do{p={x:Math.floor(Math.random()*COLS),y:Math.floor(Math.random()*ROWS)};t++;}while(t<500&&avoid.some(a=>a.x===p.x&&a.y===p.y));return p;};
const occ = g=>[...g.snake,...g.foods,...g.walls,...g.powerups,...g.speedPads,
  ...g.portals.flatMap(p=>[{x:p.x1,y:p.y1},{x:p.x2,y:p.y2}]),
  ...(g.aiSnake||[]),...(g.fallingLetters||[])];
const EVENT_TYPES=["wildcard","poison","bounty","decay","drought"];
const EVENT_DUR={wildcard:20000,poison:20000,bounty:25000,decay:20000,drought:10000};
const LEVELS=[
  {target:300,spd:250,walls:0,pads:0,portals:0},
  {target:700,spd:235,walls:0,pads:2,portals:1},
  {target:1200,spd:220,walls:0,pads:2,portals:1},
  {target:2000,spd:205,walls:2,pads:2,portals:1},
  {target:3000,spd:190,walls:3,pads:3,portals:2},
  {target:4500,spd:175,walls:4,pads:3,portals:2},
  {target:6500,spd:160,walls:5,pads:3,portals:3},
  {target:9000,spd:145,walls:6,pads:4,portals:3},
];
const WORD_TIMER = 10;
const LEVEL_DURATION = 300;
const AI_START_LEVEL = 4;
const FALLING_START_LEVEL = 5;
const BOOST_INTERVAL = 25000;
const BOOST_DURATION = 3000;
const BOOST_SPEED_MULT = 0.4;
const FALLING_SPAWN_INTERVAL = 3500;
const FALLING_SPEED = 700;
const PORTAL_REPOSITION_INTERVAL = 12000;
const DICT_URLS = [
  "https://raw.githubusercontent.com/dwyl/english-words/master/words.txt",
  "https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt",
  "https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt"
];

const F = "'Inter', 'SF Pro Display', -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif";
const FM = "'SF Mono', 'Cascadia Code', 'Fira Code', 'JetBrains Mono', 'Consolas', monospace";

const wrapDist = (a, b, max) => { const d = Math.abs(a - b); return Math.min(d, max - d); };

const moveAiSnake = (g, now) => {
  if (!g.aiSnake || g.aiSnake.length === 0) return;
  if (now - g.aiLastMove < g.aiSpeed) return;
  g.aiLastMove = now;
  const head = g.aiSnake[0];
  const vowelFoods = g.foods.filter(f => VOWELS.has(f.letter) && f.type === "normal");
  let bestDir = g.aiDir;
  if (vowelFoods.length > 0 && Math.random() < 0.85) {
    let nearest = null, minDist = Infinity;
    for (const f of vowelFoods) {
      const dist = wrapDist(f.x, head.x, COLS) + wrapDist(f.y, head.y, ROWS);
      if (dist < minDist) { minDist = dist; nearest = f; }
    }
    if (nearest) {
      const dx = nearest.x - head.x, dy = nearest.y - head.y;
      const candidates = [];
      if (dx !== 0) candidates.push(Math.abs(dx) <= COLS / 2 ? (dx > 0 ? DIR.RIGHT : DIR.LEFT) : (dx > 0 ? DIR.LEFT : DIR.RIGHT));
      if (dy !== 0) candidates.push(Math.abs(dy) <= ROWS / 2 ? (dy > 0 ? DIR.DOWN : DIR.UP) : (dy > 0 ? DIR.UP : DIR.DOWN));
      if (candidates.length === 0) candidates.push(...DIRS_ARR);
      for (const d of candidates) {
        const nx = (head.x + d.x + COLS) % COLS, ny = (head.y + d.y + ROWS) % ROWS;
        if (!g.aiSnake.some((s, i) => i > 0 && s.x === nx && s.y === ny) && !g.walls.some(w => w.x === nx && w.y === ny)) {
          bestDir = d; break;
        }
      }
    }
  } else {
    const shuffled = [...DIRS_ARR].sort(() => Math.random() - 0.5);
    for (const d of shuffled) {
      const nx = (head.x + d.x + COLS) % COLS, ny = (head.y + d.y + ROWS) % ROWS;
      if (!g.aiSnake.some((s, i) => i > 0 && s.x === nx && s.y === ny) && !g.walls.some(w => w.x === nx && w.y === ny)) {
        bestDir = d; break;
      }
    }
  }
  g.aiDir = bestDir;
  const nx = (head.x + g.aiDir.x + COLS) % COLS, ny = (head.y + g.aiDir.y + ROWS) % ROWS;
  if (g.aiSnake.some((s, i) => i > 0 && s.x === nx && s.y === ny)) return;
  g.aiSnake.unshift({ x: nx, y: ny });
  let ate = false;
  for (let i = 0; i < g.foods.length; i++) {
    const f = g.foods[i];
    if (f.x === nx && f.y === ny && VOWELS.has(f.letter) && f.type === "normal") {
      let nl; do { nl = rL(); } while (VOWELS.has(nl));
      g.foods[i] = { ...rP(occ(g)), letter: nl, type: "normal" };
      ate = true; break;
    }
  }
  if (!ate) g.aiSnake.pop();
  if (g.aiSnake.length > 14) g.aiSnake = g.aiSnake.slice(0, 14);
};

const repositionPortalsNearVowels = (g, now) => {
  if (g.portals.length === 0 || now - g.portalMoveTimer < PORTAL_REPOSITION_INTERVAL) return;
  g.portalMoveTimer = now;
  const vowelFoods = g.foods.filter(f => VOWELS.has(f.letter) && f.type === "normal");
  if (vowelFoods.length === 0) return;
  const target = vowelFoods[Math.floor(Math.random() * vowelFoods.length)];
  const pi = Math.floor(Math.random() * g.portals.length);
  const offsets = [{ x: 1, y: 0 }, { x: -1, y: 0 }, { x: 0, y: 1 }, { x: 0, y: -1 }, { x: 1, y: 1 }, { x: -1, y: -1 }, { x: 2, y: 0 }, { x: 0, y: 2 }];
  const off = offsets[Math.floor(Math.random() * offsets.length)];
  g.portals[pi].x1 = (target.x + off.x + COLS) % COLS;
  g.portals[pi].y1 = (target.y + off.y + ROWS) % ROWS;
};

export default function Game() {
  const cvs = useRef(null);
  const gs = useRef(null);
  const raf = useRef(null);
  const wtRef = useRef(null);
  const dictRef = useRef(null);
  const [loading, setLoading] = useState(true);
  const [loadStatus, setLoadStatus] = useState("Fetching dictionary...");
  const [ui, setUi] = useState({
    started:false,gameOver:false,wordMode:false,wordInput:"",feedback:null,
    score:0,len:2,collected:[],wordsFormed:[],level:1,targetScore:300,
    event:null,eventTimer:0,combo:0,stickyTimer:0,stickyLetter:"",
    poisonTimers:[],checking:false,wordTimeLeft:WORD_TIMER,transitioning:false,
    levelTimeLeft:LEVEL_DURATION,uniqueWordCount:0,boosts:0,boostActive:false,
    scoreReached:false,hasAi:false,hasFalling:false,gameOverReason:""
  });

  useEffect(()=>{
    const link = document.createElement('link');
    link.href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&family=JetBrains+Mono:wght@400;500;600;700;800&display=swap';
    link.rel = 'stylesheet';
    document.head.appendChild(link);
    return () => document.head.removeChild(link);
  }, []);

  useEffect(()=>{
    let cancelled = false;
    (async()=>{
      const words = new Set();
      for(const url of DICT_URLS){
        if(words.size > 10000) break;
        try {
          const label = url.split("/").pop();
          setLoadStatus(`Fetching ${label}...`);
          const r = await fetch(url, {signal: AbortSignal.timeout(20000)});
          if(!r.ok) continue;
          const txt = await r.text();
          txt.split(/\r?\n/).forEach(w=>{
            const lw = w.trim().toLowerCase();
            if(lw.length>=3 && lw.length<=10 && /^[a-z]+$/.test(lw)) words.add(lw);
          });
          if(words.size > 10000) {
            setLoadStatus(`${words.size.toLocaleString()} words loaded`);
            break;
          }
        } catch(e) { console.warn("Dict fetch failed:", url, e); }
      }
      if(!cancelled){
        if(words.size > 1000){
          dictRef.current = words;
          setLoadStatus(`${words.size.toLocaleString()} words loaded`);
        } else {
          dictRef.current = new Set("ace,act,add,age,ago,aid,aim,air,all,and,ant,any,ape,arc,are,ark,arm,art,ash,ask,ate,awe,axe,bad,bag,ban,bar,bat,bay,bed,bee,bet,bid,big,bit,bow,box,boy,bud,bug,bun,bus,but,buy,cab,can,cap,car,cat,cop,cow,cry,cub,cup,cut,dad,dam,day,den,dew,did,die,dig,dim,dip,doc,doe,dog,don,dot,dry,dub,dud,due,dug,dun,duo,dye,ear,eat,eel,egg,ego,elm,emu,end,era,eve,ewe,eye,fan,far,fat,fax,fed,fee,few,fig,fin,fir,fit,fix,fly,foe,fog,for,fox,fry,fun,fur,gag,gap,gas,gel,gem,get,gin,god,got,gum,gun,gut,guy,gym,had,ham,has,hat,hay,hen,her,hew,hex,hid,him,hip,his,hit,hoe,hog,hop,hot,how,hub,hue,hug,hum,hut,ice,icy,ill,imp,ink,inn,ion,ire,irk,ivy,jab,jam,jar,jaw,jay,jet,jig,job,jog,jot,joy,jug,jut,keg,key,kid,kin,kit,lab,lad,lag,lap,law,lay,lea,led,leg,let,lid,lie,lip,lit,log,lot,low,lug,mad,man,map,mar,mat,maw,max,may,men,met,mid,mix,mob,mod,mom,mop,mow,mud,mug,mum,nab,nag,nap,nay,net,new,nil,nip,nit,nod,nor,not,now,nun,nut,oak,oar,oat,odd,ode,off,oil,old,one,opt,orb,ore,our,out,owe,owl,own,pad,pal,pan,par,pat,paw,pay,pea,peg,pen,pep,per,pet,pew,pie,pig,pin,pit,ply,pod,pop,pot,pro,pry,pub,pug,pun,pup,put,rag,ram,ran,rap,rat,raw,ray,red,ref,rib,rid,rig,rim,rip,rob,rod,roe,rot,row,rub,rug,rum,run,rut,rye,sac,sad,sag,sap,sat,saw,say,sea,set,sew,she,shy,sin,sip,sir,sit,six,ski,sky,sly,sob,sod,son,sow,soy,spa,spy,sty,sub,sue,sum,sun,sup,tab,tad,tag,tan,tap,tar,tax,tea,ten,the,thy,tic,tie,tin,tip,toe,ton,too,top,tot,tow,toy,try,tub,tug,two,urn,use,van,vat,vet,via,vie,vim,vow,wad,wag,war,was,wax,way,web,wed,wet,who,why,wig,win,wit,woe,wok,won,woo,wow,yak,yam,yap,yaw,yea,yes,yet,yew,you,zap,zen,zip,zoo,able,ache,acid,acre,aged,aide,ally,also,arch,area,army,auto,avid,away,axle,back,bail,bait,bake,bald,bale,ball,band,bane,bang,bank,bare,bark,barn,base,bash,bass,bath,bead,beak,beam,bean,bear,beat,beef,been,beer,bell,belt,bend,bent,best,bike,bill,bind,bird,bite,bled,blew,blob,blog,blot,blow,blue,blur,boar,boat,body,bold,bolt,bomb,bond,bone,book,boom,boot,bore,born,boss,both,bout,bowl,bulb,bulk,bull,bump,burn,bury,bush,bust,busy,buzz,cafe,cage,cake,calf,call,calm,came,camp,cane,cape,card,care,cart,case,cash,cast,cave,cell,chat,chef,chin,chip,chop,cite,city,clad,clam,clan,clap,claw,clay,clip,clog,club,clue,coal,coat,code,coil,coin,cold,colt,comb,come,cone,cook,cool,cope,copy,cord,core,cork,corn,cost,cozy,crab,crew,crop,crow,cube,cult,curb,cure,curl,cute,dame,damp,dare,dark,darn,dart,dash,data,dawn,dead,deaf,deal,dear,deck,deed,deem,deep,deer,dent,deny,desk,dial,dice,diet,dime,dine,dire,dirt,disc,dish,dock,does,dome,done,doom,door,dose,dove,down,doze,drab,drag,draw,drew,drip,drop,drug,drum,dual,duck,dude,duel,dull,dumb,dump,dune,dunk,dupe,dusk,dust,duty,each,earl,earn,ease,east,easy,edge,edit,else,emit,envy,epic,even,ever,evil,exam,face,fact,fade,fail,fair,fake,fall,fame,fang,fare,farm,fast,fate,fawn,fear,feat,feed,feel,fell,felt,fern,feud,file,fill,film,find,fine,fire,firm,fish,fist,five,flag,flap,flat,flaw,flea,fled,flew,flex,flip,flog,flop,flow,foam,foal,foil,fold,folk,fond,font,food,fool,foot,ford,fore,fork,form,fort,foul,four,fowl,free,frog,from,fuel,full,fume,fund,fuse,fuss,gain,gait,gale,gall,game,gang,gape,garb,gash,gasp,gate,gave,gaze,gear,gene,gift,girl,gist,give,glad,glee,glen,glob,glow,glue,glum,gnaw,goad,goal,goat,goes,gold,golf,gone,good,gore,grab,gram,gray,grew,grid,grim,grin,grip,grit,grow,grub,gulf,gull,gulp,gush,gust,guts,hack,hail,hair,hale,half,hall,halt,hand,hang,hard,hare,harm,harp,hash,hate,haul,have,haze,hazy,head,heal,heap,hear,heat,heed,heel,heir,held,hell,helm,help,herb,herd,here,hero,hide,high,hike,hill,hilt,hind,hint,hire,hiss,hive,hoax,hold,hole,holy,home,hone,hood,hook,hoop,hope,horn,hose,host,hour,howl,huge,hull,hump,hung,hunt,hurl,hurt,hush,hymn,icon,idea,idle,idol,inch,into,iris,iron,isle,item,jack,jade,jail,jazz,jeer,jerk,jest,jive,join,joke,jolt,jump,junk,jury,just,keen,keep,kelp,kept,kick,kill,kind,king,kiss,kite,knee,knew,knit,knob,knot,know,lace,lack,laid,lair,lake,lamb,lame,lamp,land,lane,lard,lark,lash,lass,last,late,lawn,lazy,lead,leaf,leak,lean,leap,left,lend,lens,less,liar,lick,lied,lies,life,lift,like,limb,lime,limp,line,link,lint,lion,lips,list,live,load,loaf,loam,loan,lock,loft,logo,lone,long,look,loom,loop,loot,lord,lore,lose,loss,lost,loud,love,luck,lull,lump,lung,lure,lurk,lush,lust,made,maid,mail,main,make,male,mall,malt,mane,many,mare,mark,mash,mask,mass,mast,mate,math,maze,meal,mean,meat,meet,meld,melt,memo,mend,menu,mere,mesh,mess,mild,mile,milk,mill,mime,mind,mine,mint,mire,miss,mist,moan,moat,mock,mode,mold,mole,monk,mood,moon,moor,more,moss,most,moth,move,much,muck,mule,mull,muse,mush,musk,must,mute,myth,nail,name,nape,nave,navy,near,neat,neck,need,nest,news,next,nice,nick,nine,node,none,noon,norm,nose,note,noun,nude,numb,sick,sack,sock,suck,slap,slam,slim,slid,slip,slit,slug,slum,scan,scar,seam,seal,seed,seek,seem,seen,self,sell,send,sent,shed,shin,ship,shoe,shop,shot,show,shut,side,sift,sigh,sign,silk,sill,silt,sing,sink,sire,site,size,skit,slab,slag,slaw,sled,slew,slob,slog,slop,slot,slow,slur,smog,snap,snip,snob,snot,snow,snub,snug,soak,soap,soar,soda,sofa,soft,soil,sold,sole,some,song,soon,soot,sore,sort,soul,sour,span,spar,spec,sped,spin,spit,spot,spry,spur,stab,stag,star,stay,stem,step,stew,stir,stop,stub,stud,stun,such,suit,sulk,sung,sunk,sure,surf,swan,swap,sway,swim,tack,tact,tail,take,tale,talk,tall,tame,tang,tank,tape,tart,task,taxi,teak,teal,team,tear,tell,tend,tent,term,test,text,than,that,them,then,they,thin,this,thou,thud,thug,thus,tick,tide,tidy,tier,tile,till,tilt,time,tine,tiny,tire,toad,toil,told,toll,tomb,tome,tone,took,tool,tops,tore,torn,toss,tour,town,trap,tray,tree,trek,trim,trio,trip,trod,trot,true,tuba,tube,tuck,tuft,tuna,tune,turf,turn,tusk,twin,type,ugly,undo,unit,unto,upon,urge,used,user,vain,vale,vane,vary,vase,vast,veil,vein,vent,verb,very,vest,veto,vice,view,vile,vine,visa,void,volt,vote,wade,wage,wail,wait,wake,walk,wand,want,ward,warm,warn,warp,wart,wary,wash,wasp,wave,wavy,waxy,weak,wean,wear,weed,week,weep,weld,well,went,wept,were,west,what,when,whim,whip,whom,wick,wide,wife,wild,will,wilt,wily,wind,wine,wing,wink,wipe,wire,wise,wish,wisp,with,woke,wolf,wood,wool,word,wore,work,worm,worn,wove,wrap,wren,writ,yank,yard,yarn,year,yell,yoga,yoke,your,zeal,zero,zinc,zone,zoom,oath,obey,odds,omit,once,only,onto,open,oral,ours,oust,oven,over,pace,pack,pact,page,paid,pail,pain,pair,pale,palm,pane,pang,park,part,pass,past,path,pave,peak,pear,peat,peek,peel,peer,pelt,perk,pest,pick,pier,pike,pile,pine,pink,pipe,plan,play,plea,plod,plot,plow,plug,plum,plus,pock,poem,poet,poke,pole,poll,polo,pomp,pond,pony,pool,poor,pope,pore,pork,port,pose,post,pour,pray,prep,prey,prod,prop,pros,prow,pull,pulp,pump,punk,pure,push,quiz,race,rack,raft,rage,raid,rail,rain,rake,ramp,rang,rank,rare,rash,rate,rave,read,real,reap,rear,reed,reef,reel,rely,rend,rent,rest,rice,rich,ride,rift,ring,riot,rise,risk,road,roam,roar,robe,rock,rode,role,roll,roof,room,root,rope,rose,rosy,rote,rout,rude,ruin,rule,rump,rung,rush,rust,safe,sage,said,sail,sake,sale,salt,same,sand,sane,sang,sank,sash,save,scam,scar,seal,seam,sear,seat,sect,seed,seek,seem,seen,self,sell,send,sent,sept,shed,shin,ship,shoe,shoo,shop,shot,show,shut,side,sift,sigh,sign,silk,sill,silt,sine,sing,sink,sire,site,size,skit,slab,slag,slam,slap,slat,slaw,sled,slew,slid,slim,slip,slit,slob,slog,slop,slot,slow,slug,slum,slur,smog,snap,snip,snob,snot,snow,snub,snug,soak,soap,soar,soda,sofa,soft,soil,sold,sole,some,song,soon,soot,sore,sort,soul,sour,span,spar,spec,sped,spin,spit,spot,spry,spur,stab,stag,star,stay,stem,step,stew,stir,stop,stub,stud,stun,such,suck,suit,sulk,sung,sunk,sure,surf,swan,swap,sway,swim,tabs,tack,tact,tail,take,tale,talk,tall,tame,tang,tank,tape,tarn,tart,task,taxi,teak,teal,team,tear,teem,tell,temp,tend,tent,term,test,text,than,that,them,then,they,thin,this,thou,thud,thug,thus,tick,tide,tidy,tier,tile,till,tilt,time,tine,tiny,tire,toad,toil,told,toll,tomb,tome,tone,took,tool,tops,tore,torn,tort,toss,tour,town,trap,tray,tree,trek,trim,trio,trip,trod,trot,true,tuba,tube,tuck,tuft,tuna,tune,turf,turn,tusk,twin,type,ugly,undo,unit,unto,upon,urge,used,user,vain,vale,vane,vary,vase,vast,veil,vein,vent,verb,very,vest,veto,vice,view,vile,vine,visa,void,volt,vote,wade,wage,wail,wait,wake,walk,wand,want,ward,warm,warn,warp,wart,wary,wash,wasp,wave,wavy,waxy,weak,wean,wear,weed,week,weep,weld,well,went,wept,were,west,what,when,whim,whip,whom,wick,wide,wife,wild,will,wilt,wily,wind,wine,wing,wink,wipe,wire,wise,wish,wisp,with,woke,wolf,wood,wool,word,wore,work,worm,worn,wove,wrap,wren,writ,yank,yard,yarn,year,yell,yoga,yoke,your,zeal,zero,zinc,zone,zoom,abort,about,above,abuse,ached,acorn,acres,acute,adapt,added,adept,admit,adobe,adopt,adult,after,again,agent,agile,aging,agree,ahead,aisle,alarm,album,alien,align,alike,alive,allay,alley,allot,allow,alloy,aloft,alone,along,aloof,alpha,altar,alter,ample,angel,anger,angle,angry,angst,anime,ankle,annex,anvil,apart,apple,apply,arena,argue,arise,armor,arose,array,arrow,arson,aside,asset,atlas,attic,audio,audit,avert,avian,avoid,await,awake,award,aware,awful,bacon,badge,badly,bagel,baker,based,basic,basin,basis,batch,beach,beans,beast,begin,being,below,bench,berry,bible,bigot,binge,birth,black,blade,blame,bland,blank,blare,blast,blaze,bleak,bleat,bleed,blend,bless,blind,blink,bliss,blitz,bloat,block,bloke,blood,bluff,blunt,board,boast,bonus,booth,booty,bound,brace,braid,brain,brake,brand,brash,brass,brave,bravo,brawl,bread,break,breed,brick,bride,brief,brine,bring,brink,brisk,broad,broil,broke,brook,broth,brown,brush,brute,budge,build,built,bulge,bunch,burst,buyer,cabal,cabin,cable,cache,cadet,candy,cargo,carry,carve,catch,cater,cause,cease,cedar,chain,chair,chalk,champ,chant,chaos,charm,chart,chase,cheap,cheat,check,cheek,cheer,chess,chest,chief,child,chill,china,chord,chose,chunk,churn,circa,civic,civil,claim,clamp,clash,clasp,class,clean,clear,clerk,click,cliff,climb,cling,cloak,clock,clone,close,cloth,cloud,clown,coach,coast,cobra,cocoa,colon,color,comic,coral,corps,couch,could,count,coupe,court,cover,crack,craft,cramp,crane,crash,crate,crave,crawl,craze,crazy,creak,cream,creek,creep,crest,crime,crisp,cross,crowd,crown,crude,cruel,crush,cubic,curve,cyber,cycle,dairy,dance,datum,dealt,death,debit,debug,debut,decal,decay,decor,decoy,decry,defer,deity,delay,delta,delve,demon,denim,dense,depot,depth,derby,detox,devil,diary,digit,diner,dirty,disco,dodge,donor,doubt,dough,draft,drain,drake,drama,drank,drape,drawn,dread,dream,dress,dried,drift,drill,drink,drive,drone,droit,drool,droop,drove,drown,drugs,drunk,dryer,dying,eager,early,earth,easel,eight,eject,elder,elect,elite,email,ember,emery,emoji,empty,enact,endow,enemy,enjoy,ensue,enter,entry,envoy,epoch,equal,equip,erase,erode,error,essay,ethos,evade,event,every,evict,evoke,exact,exalt,excel,exert,exile,exist,expat,expel,extra,exude,fable,facet,faint,fairy,faith,fancy,fatal,fatty,fault,fauna,feast,femur,fence,ferry,fetch,fever,fiber,field,fiend,fifth,fifty,fight,filth,final,finch,first,flame,flank,flare,flash,flask,flesh,flick,fling,flint,float,flock,flood,floor,flora,flour,flout,flown,fluid,fluke,flung,flush,flute,focal,focus,foggy,folly,force,forge,forte,forth,forum,found,foyer,frail,frame,frank,fraud,fresh,friar,front,frost,froze,fruit,fully,fungi,funny,gamma,gauge,genre,ghost,giant,given,gland,glare,glass,gleam,glide,globe,gloom,glory,gloss,glove,going,goose,gorge,gotta,gouge,grace,grade,graft,grain,grand,grant,grasp,grass,grate,grave,gravy,graze,great,greed,greek,green,greet,grief,grill,grind,gripe,groan,groom,grope,gross,group,grove,growl,grown,gruel,guard,guava,guess,guest,guide,guild,guilt,guise,gulch,gully,gumbo,gusto,hairy,happy,hardy,harsh,haste,hasty,hatch,haunt,haven,havoc,heart,heave,heavy,hedge,heist,hence,herbs,heron,hinge,hippo,hitch,hobby,homer,honey,honor,horns,horse,hotel,hotly,hound,house,hover,human,humid,humor,hurry,hyena,hyper,ideal,image,imply,inane,index,indie,inert,infer,ingot,inner,input,inter,intro,ionic,irate,ivory,jewel,joint,joker,jolly,judge,juice,juicy,jumbo,juror,karma,kayak,kebab,kitty,knack,knead,kneel,knelt,knife,knock,knoll,known,label,labor,lance,large,laser,latch,later,laugh,layer,leach,leafy,learn,lease,leash,least,leave,ledge,legal,lemon,level,lever,light,lilac,limit,linen,liner,lingo,llama,lodge,lofty,logic,login,loose,lorry,lotus,lower,loyal,lucid,lucky,lunar,lunch,lunge,lying,lyric,macro,mafia,magic,major,maker,mange,mango,manor,maple,march,marry,marsh,match,mayor,medal,media,melee,melon,mercy,merge,merit,merry,metal,meter,midst,might,minor,minus,mirth,miser,model,modem,mogul,moist,money,month,moose,moral,motif,motor,motto,mound,mount,mourn,mouse,mouth,movie,muddy,mural,music,naive,nanny,nerve,never,niche,night,nimby,ninja,noble,noise,north,notch,noted,novel,nudge,nurse,nylon,occur,ocean,offer,often,olive,omega,onset,opera,optic,orbit,order,organ,other,otter,ought,ounce,outer,outdo,overt,oxide,ozone,panel,panic,paper,paste,patch,pause,peace,peach,pedal,penny,perch,peril,perky,phase,phone,photo,piano,piece,pilot,pinch,pitch,pixel,pizza,place,plaid,plain,plane,plank,plant,plate,plaza,plead,pleat,pluck,plumb,plume,plump,plunk,plush,poach,point,poise,polar,poker,poppy,porch,posse,pound,power,prank,prawn,press,price,pride,prime,print,prior,prism,prize,probe,prong,prone,proof,prose,proud,prove,prowl,prude,prune,psalm,pulse,punch,pupil,purge,purse,quail,qualm,quart,queen,query,quest,queue,quick,quiet,quilt,quirk,quota,quote,rabbi,radar,radio,rainy,raise,rally,ranch,range,rapid,ratio,raven,reach,react,realm,rebel,reign,relax,relay,remix,repay,repel,reply,retry,revel,ridge,rifle,rigid,rigor,rinse,risen,risky,rival,river,rivet,roast,robot,rocky,rodeo,rogue,roman,roost,rouge,rough,round,route,rover,royal,rumor,rural,rusty,saber,sadly,safer,saint,salad,salon,salsa,salve,sandy,sauce,sauna,savor,scale,scalp,scant,scare,scarf,scary,scene,scent,scoff,scold,scone,scoop,scope,score,scout,scowl,scram,scrap,serve,setup,seven,shade,shady,shaft,shake,shaky,shall,shame,shape,share,shark,sharp,shave,shawl,shear,sheen,sheep,sheer,sheet,shelf,shell,shift,shine,shiny,shire,shirt,shock,shore,short,shout,shove,shrub,shrug,siege,sight,sigma,since,sixty,skate,skill,skimp,skull,skunk,slash,slate,slave,sleek,sleep,sleet,slice,slide,slime,sling,slope,sloth,small,smart,smash,smell,smile,smirk,smith,smoke,snack,snail,snare,sneak,sneer,snore,snout,solar,solid,solve,sonic,sorry,sound,south,space,spare,spark,spawn,speak,spear,speed,spell,spend,spice,spicy,spill,spine,spoke,spoon,sport,spray,squad,stack,staff,stage,stain,stair,stake,stale,stalk,stall,stamp,stand,stank,stare,stark,start,state,stave,stays,steak,steal,steam,steel,steep,steer,stern,stick,stiff,still,sting,stink,stock,stoic,stoke,stomp,stone,stood,stool,stoop,store,storm,story,stout,stove,strap,straw,stray,strip,strum,strut,stuck,study,stuff,stump,stung,stunk,stunt,style,sugar,suite,super,surge,sushi,swamp,swarm,swear,sweat,sweep,sweet,swept,swift,swing,swipe,swirl,sworn,swung,syrup,tabby,table,tacit,taint,taken,talon,taste,tasty,taunt,tempo,tense,tenth,tepid,terra,theft,their,theme,there,thick,thief,thigh,thing,think,third,thorn,those,three,threw,throw,thumb,tiger,tight,timer,tired,titan,title,toast,today,token,topic,torch,total,touch,tough,towel,tower,toxic,trace,track,trade,trail,train,trait,tramp,trash,tread,treat,trend,trial,tribe,trick,trike,tripe,trite,troll,troop,trout,truck,truly,trunk,trust,truth,tuber,tumor,tuner,tunic,turbo,tutor,tweed,twang,tweak,tweet,twice,twill,twine,twirl,twist,tying,ultra,uncle,under,undid,undue,unfit,unify,union,unite,unity,unlit,until,upper,upset,urban,usher,usual,utter,vague,valid,valor,value,valve,vapor,vault,vegan,veins,venue,verge,verse,vigor,vinyl,viola,viper,viral,virus,visor,visit,vista,vital,vivid,vocal,vodka,vogue,voice,voila,voter,vouch,vowel,wager,wages,wagon,waist,waste,watch,water,weary,weave,wedge,weird,whale,wheat,wheel,where,which,while,whine,whirl,white,whole,whose,wield,witch,woman,women,world,worry,worse,worst,worth,would,wound,wrack,wrath,wreak,wreck,wring,wrist,write,wrong,wrote,yacht,yearn,yeast,yield,young,youth,zebra,zesty".split(","));
          setLoadStatus(`Using fallback dictionary`);
        }
        setLoading(false);
      }
    })();
    return()=>{cancelled=true;};
  },[]);

  const isValidWord = useCallback((w)=>{
    return dictRef.current ? dictRef.current.has(w) : false;
  },[]);

  const initGame = useCallback((lvl=0, keepScore=false)=>{
    const snake=[];const sx=Math.floor(COLS/2),sy=Math.floor(ROWS/2);
    for(let i=0;i<2;i++)snake.push({x:sx-i,y:sy});
    const L=LEVELS[Math.min(lvl,LEVELS.length-1)];
    const foods=[{...rP(snake),letter:rL(),type:"normal"},{...rP(snake),letter:rL(),type:"normal"}];
    const all=[...snake,...foods];
    const walls=[];for(let i=0;i<L.walls;i++){const w=rP(all);walls.push({...w,dx:Math.random()<0.5?1:-1,dy:0});all.push(w);}
    const speedPads=[];for(let i=0;i<L.pads;i++){const s=rP(all);speedPads.push({...s,active:0});all.push(s);}
    const portals=[];for(let i=0;i<L.portals;i++){const p1=rP(all);all.push(p1);const p2=rP(all);all.push(p2);portals.push({x1:p1.x,y1:p1.y,x2:p2.x,y2:p2.y});}
    const ps=keepScore&&gs.current?gs.current.score:0;
    const pw=keepScore&&gs.current?gs.current.wordsFormed:[];
    const puw=keepScore&&gs.current?gs.current.uniqueWords:new Set();
    const pboosts=keepScore&&gs.current?gs.current.boosts:0;
    const lifelineCooldown = 18000 + lvl * 6000 + Math.random() * (10000 + lvl * 5000);
    const now = performance.now();
    const hasAi = lvl >= AI_START_LEVEL;
    const hasFalling = lvl >= FALLING_START_LEVEL;
    let aiSnake = [], aiDir = DIR.LEFT, aiSpeed = Math.max(200, 350 - lvl * 20);
    if (hasAi) {
      const ax = Math.floor(COLS * 0.8), ay = Math.floor(ROWS * 0.2);
      aiSnake = [{ x: ax, y: ay }, { x: ax + 1, y: ay }];
    }
    gs.current={snake,dir:DIR.RIGHT,nextDir:DIR.RIGHT,foods,collected:[],score:ps,
      gameOver:false,wordMode:false,baseSpeed:L.spd,speed:L.spd,lastMove:0,flash:null,
      wordsFormed:[...pw],uniqueWords:puw,gameStart:now,level:lvl,levelStartTime:now,
      currentEvent:null,lastEventTime:now,eventCooldown:40000+Math.random()*20000,
      stickyUntil:0,stickyLetter:"",walls,speedPads,portals,powerups:[],powerupTimer:now+20000,
      comboCount:0,lastWordTime:0,poisonLetters:[],wildcardActive:false,bountyWord:null,
      decayActive:false,decayTimer:0,wallMoveInterval:Math.max(400,800-lvl*60),wallLastMove:0,
      lifelineTimer:now+lifelineCooldown,lifelineOnGrid:null,
      aiSnake,aiDir,aiSpeed,aiLastMove:now,
      fallingLetters:[],lastFallingSpawn:now,
      boosts:pboosts,boostActiveUntil:0,lastBoostGrant:now,
      portalMoveTimer:now,
      levelAdvancePending:false};
    setUi({started:true,gameOver:false,wordMode:false,wordInput:"",feedback:null,
      score:ps,len:2,collected:[],level:lvl+1,targetScore:L.target,
      event:null,eventTimer:0,combo:0,stickyTimer:0,stickyLetter:"",
      poisonTimers:[],checking:false,wordTimeLeft:WORD_TIMER,
      wordsFormed:[...pw],transitioning:false,
      levelTimeLeft:LEVEL_DURATION,uniqueWordCount:puw.size,boosts:pboosts,boostActive:false,
      scoreReached:false,hasAi,hasFalling,gameOverReason:""});
  },[]);

  const hasLetters=(word,coll)=>{
    const pool=[...coll];let wc=pool.filter(c=>c==="★").length;
    for(const ch of word.toUpperCase()){const i=pool.indexOf(ch);if(i!==-1)pool.splice(i,1);else if(wc>0){wc--;const wi=pool.indexOf("★");if(wi!==-1)pool.splice(wi,1);}else return false;}return true;};

  const exitWordMode=useCallback(()=>{
    if(wtRef.current){clearInterval(wtRef.current);wtRef.current=null;}
    if(gs.current)gs.current.wordMode=false;
    setUi(p=>({...p,wordMode:false,wordInput:"",feedback:null,wordTimeLeft:WORD_TIMER}));
  },[]);

  const onWordTimeout=useCallback(()=>{
    if(wtRef.current){clearInterval(wtRef.current);wtRef.current=null;}
    const g=gs.current;if(!g)return;g.wordMode=false;
    const penalty = g.collected.length;
    const newLen = g.snake.length + penalty;
    const tail=g.snake[g.snake.length-1];while(g.snake.length<newLen)g.snake.push({...tail});
    g.flash={type:"bad",time:40};
    setUi(p=>({...p,wordMode:false,wordInput:"",wordTimeLeft:WORD_TIMER,len:g.snake.length,
      feedback:{type:"penalty",msg:`Time's up! +${penalty} from unused letters`}}));
    setTimeout(()=>setUi(p=>({...p,feedback:null})),3000);
  },[]);

  const tryLevelAdvance = useCallback((g) => {
    if (!g || g.levelAdvancePending) return;
    const lvlIdx = g.level;
    const nextTarget = LEVELS[Math.min(lvlIdx, LEVELS.length - 1)].target;
    if (g.score >= nextTarget && g.collected.length === 0 && lvlIdx < LEVELS.length) {
      g.levelAdvancePending = true;
      setTimeout(() => {
        setUi(p => ({ ...p, transitioning: true, feedback: { type: "success", msg: `Level Up! Entering Level ${lvlIdx + 2}` } }));
        setTimeout(() => initGame(lvlIdx + 1, true), 2500);
      }, 1000);
    }
  }, [initGame]);

  const submitWord=useCallback((word)=>{
    const g=gs.current;if(!g)return;
    const w=word.toLowerCase().trim();
    if(w.length<3){setUi(p=>({...p,feedback:{type:"error",msg:"Minimum 3 letters"}}));return;}
    if(!hasLetters(w,g.collected)){setUi(p=>({...p,feedback:{type:"error",msg:"You don't have those letters"}}));return;}
    if(!isValidWord(w)){setUi(p=>({...p,feedback:{type:"error",msg:`"${w.toUpperCase()}" -- not in dictionary`}}));return;}
    if(wtRef.current){clearInterval(wtRef.current);wtRef.current=null;}

    const used=[...w.toUpperCase()];const rem=[...g.collected];
    for(const ch of used){let i=rem.indexOf(ch);if(i===-1)i=rem.indexOf("★");if(i!==-1)rem.splice(i,1);}
    const unusedCount = rem.length;
    g.collected=rem;
    g.poisonLetters=g.poisonLetters.filter(p=>rem.some(c=>c===p.letter));
    if(g.stickyUntil>0&&used.includes(g.stickyLetter)){g.stickyUntil=0;g.speed=g.baseSpeed;}

    g.uniqueWords.add(w);

    const shed=w.length,snakeLen=g.snake.length,halfLen=Math.ceil(snakeLen/2);
    let scrPts=0;for(const ch of w.toUpperCase())scrPts+=(SP[ch]||1);
    const now=performance.now();
    if(g.lastWordTime>0&&now-g.lastWordTime<15000)g.comboCount++;else g.comboCount=1;
    g.lastWordTime=now;const comboMult=Math.min(g.comboCount,5);
    const bountyHit=g.bountyWord&&w===g.bountyWord;
    let newLen,penalty=false;
    if(shed>=halfLen){
      newLen=Math.max(2,snakeLen-(bountyHit?shed*2:shed));
      g.score+=(scrPts*10+(shed>=5?200:0)+(bountyHit?500:0))*comboMult;
      g.flash={type:"good",time:30};
    }else{
      newLen=snakeLen+unusedCount;
      penalty=true;g.score+=scrPts*2*comboMult;g.flash={type:"bad",time:40};
    }
    if(newLen<g.snake.length)g.snake=g.snake.slice(0,newLen);
    else{const tail=g.snake[g.snake.length-1];while(g.snake.length<newLen)g.snake.push({...tail});}
    if(bountyHit)g.bountyWord=null;
    const pts=penalty?scrPts*2*comboMult:(scrPts*10+(shed>=5?200:0)+(bountyHit?500:0))*comboMult;
    g.wordsFormed.push({word:w.toUpperCase(),shed,penalty,score:pts,combo:comboMult,bounty:!!bountyHit});
    g.wordMode=false;

    const lvlIdx=g.level;const nextTarget=LEVELS[Math.min(lvlIdx,LEVELS.length-1)].target;
    const scoreReached = g.score >= nextTarget && lvlIdx < LEVELS.length;
    const lettersEmpty = g.collected.length === 0;

    let feedbackMsg;
    if (scoreReached && !lettersEmpty) {
      feedbackMsg = `Score reached! Shed all ${g.collected.length} letters to advance`;
    } else if (bountyHit) {
      feedbackMsg = `Bounty! "${w.toUpperCase()}" +${pts}  ${snakeLen}->${newLen}`;
    } else if (penalty) {
      feedbackMsg = `"${w.toUpperCase()}" shed ${shed}, needed ${halfLen} -- +${unusedCount} unused  ${snakeLen}->${newLen}`;
    } else {
      feedbackMsg = `"${w.toUpperCase()}" +${pts}${comboMult>1?` x${comboMult}`:""} shed ${shed}  ${snakeLen}->${newLen}`;
    }
    const feedbackType = scoreReached && !lettersEmpty ? "error" : bountyHit ? "bounty" : penalty ? "penalty" : "success";

    setUi(p=>({...p,wordMode:false,wordInput:"",collected:[...g.collected],score:g.score,len:g.snake.length,
      wordsFormed:[...g.wordsFormed],combo:g.comboCount,wordTimeLeft:WORD_TIMER,
      uniqueWordCount:g.uniqueWords.size,scoreReached,
      feedback:{type:feedbackType,msg:feedbackMsg}}));

    if (scoreReached && lettersEmpty) {
      tryLevelAdvance(g);
    } else {
      setTimeout(()=>setUi(p=>({...p,feedback:null})),3000);
    }
  },[initGame,isValidWord,tryLevelAdvance]);

  useEffect(()=>{
    const h=e=>{const g=gs.current;if(!g||g.gameOver){if(e.key==="Enter"&&g?.gameOver)initGame(0);return;}
      if(g.wordMode){if(e.key==="Escape")exitWordMode();return;}
      if(e.key==="w"||e.key==="W"){
        if(g.collected.length<3){setUi(p=>({...p,feedback:{type:"error",msg:"Need 3+ letters first"}}));setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);return;}
        e.preventDefault();g.wordMode=true;
        setUi(p=>({...p,wordMode:true,wordInput:"",feedback:null,wordTimeLeft:WORD_TIMER}));
        if(wtRef.current)clearInterval(wtRef.current);
        let tl=WORD_TIMER;
        wtRef.current=setInterval(()=>{tl--;setUi(p=>({...p,wordTimeLeft:tl}));if(tl<=0)onWordTimeout();},1000);
        return;
      }
      if(e.key==="b"||e.key==="B"){
        if(g.boosts>0&&g.boostActiveUntil<=performance.now()){
          g.boosts--;
          g.boostActiveUntil=performance.now()+BOOST_DURATION;
          g.speed=Math.max(60,g.baseSpeed*BOOST_SPEED_MULT);
          setUi(p=>({...p,boosts:g.boosts,boostActive:true,feedback:{type:"success",msg:`BOOST! ${(BOOST_DURATION/1000).toFixed(0)}s speed`}}));
          setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);
        }
        return;
      }
      const km={ArrowUp:"UP",ArrowDown:"DOWN",ArrowLeft:"LEFT",ArrowRight:"RIGHT"};
      const d=km[e.key];if(d){e.preventDefault();const nd=DIR[d];if(nd.x+g.dir.x!==0||nd.y+g.dir.y!==0)g.nextDir=nd;}
    };window.addEventListener("keydown",h);return()=>window.removeEventListener("keydown",h);
  },[initGame,exitWordMode,onWordTimeout]);

  useEffect(()=>{
    if(!gs.current)return;let run=true;
    const tick=ts=>{
      if(!run)return;const g=gs.current;
      if(!g||g.gameOver||g.wordMode){draw(ts);raf.current=requestAnimationFrame(tick);return;}
      const now=performance.now();

      // === LEVEL TIMER ===
      const levelElapsed = (now - g.levelStartTime) / 1000;
      const levelTimeLeft = Math.max(0, LEVEL_DURATION - levelElapsed);
      setUi(p=>({...p,levelTimeLeft:Math.ceil(levelTimeLeft)}));
      if (levelTimeLeft <= 0) {
        g.gameOver = true;
        setUi(p => ({ ...p, gameOver: true, gameOverReason: "Time's up!" }));
        draw(ts); raf.current = requestAnimationFrame(tick); return;
      }

      // === BOOST EXPIRY ===
      if (g.boostActiveUntil > 0 && now >= g.boostActiveUntil) {
        g.boostActiveUntil = 0;
        if (g.stickyUntil > 0) g.speed = g.baseSpeed * 1.8;
        else g.speed = g.baseSpeed;
        setUi(p => ({ ...p, boostActive: false }));
      }

      // === BOOST GRANT ===
      if (now - g.lastBoostGrant >= BOOST_INTERVAL) {
        g.lastBoostGrant = now;
        g.boosts++;
        setUi(p => ({ ...p, boosts: g.boosts, feedback: { type: "success", msg: `Boost stored! (${g.boosts} available) Press B` } }));
        setTimeout(() => setUi(p => ({ ...p, feedback: null })), 2500);
      }

      // === EVENTS ===
      if(!g.currentEvent&&now-g.lastEventTime>=g.eventCooldown){
        const et=EVENT_TYPES[Math.floor(Math.random()*EVENT_TYPES.length)];
        g.currentEvent={type:et,start:now,end:now+EVENT_DUR[et]};g.lastEventTime=now;g.eventCooldown=45000+Math.random()*25000;
        if(et==="wildcard"){g.foods.push({...rP(occ(g)),letter:"★",type:"wildcard"});}
        else if(et==="bounty")g.bountyWord=BOUNTY_POOL[Math.floor(Math.random()*BOUNTY_POOL.length)];
        else if(et==="decay"){g.decayActive=true;g.decayTimer=now+8000;}
        else if(et==="drought")g.foods.forEach((f,i)=>{if(VOWELS.has(f.letter)&&f.type==="normal"){let l;do{l=rL();}while(VOWELS.has(l));g.foods[i].letter=l;}});
        else if(et==="poison")g.foods.push({...rP(occ(g)),letter:rL(),type:"poison"});
        setUi(p=>({...p,event:et,eventTimer:Math.ceil(EVENT_DUR[et]/1000)}));
      }
      if(g.currentEvent){
        if(now>=g.currentEvent.end){
          if(g.currentEvent.type==="wildcard")g.foods=g.foods.filter(f=>f.type!=="wildcard");
          if(g.currentEvent.type==="poison")g.foods=g.foods.filter(f=>f.type!=="poison");
          if(g.currentEvent.type==="bounty")g.bountyWord=null;
          if(g.currentEvent.type==="decay")g.decayActive=false;
          g.currentEvent=null;setUi(p=>({...p,event:null,eventTimer:0}));
        }else{
          setUi(p=>({...p,eventTimer:Math.max(1,Math.ceil((g.currentEvent.end-now)/1000))}));
          if(g.currentEvent.type==="decay"&&g.decayActive&&now>=g.decayTimer&&g.collected.length>0){
            g.collected.shift();g.decayTimer=now+8000;
            setUi(p=>({...p,collected:[...g.collected]}));
            if (g.collected.length === 0) tryLevelAdvance(g);
          }
        }
      }
      g.poisonLetters=g.poisonLetters.filter(p=>{
        if(now>=p.deadline){const tail=g.snake[g.snake.length-1];for(let i=0;i<5;i++)g.snake.push({...tail});g.flash={type:"bad",time:30};
          setUi(pp=>({...pp,len:g.snake.length,feedback:{type:"penalty",msg:`Poison "${p.letter}" -- +5 segments`}}));setTimeout(()=>setUi(pp=>({...pp,feedback:null})),2500);return false;}return true;});
      setUi(p=>({...p,poisonTimers:g.poisonLetters.map(pl=>({letter:pl.letter,secs:Math.ceil((pl.deadline-now)/1000)}))}));
      if(g.stickyUntil>0&&now>=g.stickyUntil){g.stickyUntil=0;if(g.boostActiveUntil<=0)g.speed=g.baseSpeed;setUi(p=>({...p,stickyTimer:0,stickyLetter:""}));}
      if(g.stickyUntil>0)setUi(p=>({...p,stickyTimer:Math.max(0,Math.ceil((g.stickyUntil-now)/1000))}));
      if(g.walls.length>0&&now-g.wallLastMove>=g.wallMoveInterval){g.wallLastMove=now;g.walls.forEach(w=>{w.x=(w.x+w.dx+COLS)%COLS;});}

      const lifeChance = g.level <= 1 ? 0.65 : g.level <= 2 ? 0.5 : g.level <= 3 ? 0.4 : Math.max(0.1, 0.3 - g.level * 0.03);
      const lifelineCooldown = 18000 + g.level * 6000 + Math.random() * (10000 + g.level * 5000);
      if(!g.lifelineOnGrid&&now>=g.lifelineTimer){if(Math.random()<lifeChance)g.lifelineOnGrid={...rP(occ(g)),spawn:now};g.lifelineTimer=now+lifelineCooldown;}
      const lifelineDuration = g.level <= 1 ? 16000 : g.level <= 2 ? 14000 : 11000;
      if(g.lifelineOnGrid&&now-g.lifelineOnGrid.spawn>lifelineDuration)g.lifelineOnGrid=null;

      if(now>=g.powerupTimer){const types=["shrink","freeze"];g.powerups.push({...rP(occ(g)),type:types[Math.floor(Math.random()*types.length)],spawn:now});g.powerupTimer=now+25000+Math.random()*15000;}
      g.powerups=g.powerups.filter(p=>now-p.spawn<15000);

      // === PORTAL REPOSITIONING NEAR VOWELS ===
      repositionPortalsNearVowels(g, now);

      // === AI SNAKE ===
      if (g.level >= AI_START_LEVEL) moveAiSnake(g, now);

      // === FALLING LETTERS ===
      if (g.level >= FALLING_START_LEVEL) {
        if (now - g.lastFallingSpawn >= FALLING_SPAWN_INTERVAL) {
          g.lastFallingSpawn = now;
          const fx = Math.floor(Math.random() * COLS);
          g.fallingLetters.push({ x: fx, y: 0, letter: rL(), lastFall: now, landed: false, landTime: 0 });
        }
        g.fallingLetters.forEach(fl => {
          if (!fl.landed && now - fl.lastFall >= FALLING_SPEED) {
            fl.y++;
            fl.lastFall = now;
            if (fl.y >= ROWS - 1) { fl.landed = true; fl.landTime = now; }
          }
        });
        g.fallingLetters = g.fallingLetters.filter(fl => !(fl.landed && now - fl.landTime > 4000));
      }

      // === PLAYER MOVEMENT ===
      if(ts-g.lastMove>=g.speed){
        g.lastMove=ts;g.dir=g.nextDir;
        const head=g.snake[0];let nx=(head.x+g.dir.x+COLS)%COLS,ny=(head.y+g.dir.y+ROWS)%ROWS;
        for(const pt of g.portals){if(nx===pt.x1&&ny===pt.y1){nx=pt.x2;ny=pt.y2;break;}if(nx===pt.x2&&ny===pt.y2){nx=pt.x1;ny=pt.y1;break;}}
        if(g.walls.some(w=>w.x===nx&&w.y===ny)){g.gameOver=true;setUi(p=>({...p,gameOver:true,gameOverReason:"Hit a wall!"}));draw(ts);raf.current=requestAnimationFrame(tick);return;}
        if(g.snake.some((s,i)=>i>0&&s.x===nx&&s.y===ny)){g.gameOver=true;setUi(p=>({...p,gameOver:true,gameOverReason:"Hit yourself!"}));draw(ts);raf.current=requestAnimationFrame(tick);return;}
        g.snake.unshift({x:nx,y:ny});
        if(g.lifelineOnGrid&&g.lifelineOnGrid.x===nx&&g.lifelineOnGrid.y===ny){
          const sh=Math.max(3,Math.floor(g.snake.length*0.4));g.snake=g.snake.slice(0,Math.max(2,g.snake.length-sh));g.lifelineOnGrid=null;g.flash={type:"good",time:25};
          setUi(p=>({...p,len:g.snake.length,feedback:{type:"success",msg:`Lifeline! -${sh} segments`}}));setTimeout(()=>setUi(p=>({...p,feedback:null})),2500);}
        let ate=false;
        for(let i=0;i<g.foods.length;i++){const f=g.foods[i];
          if(f.x===nx&&f.y===ny){
            if(f.type==="wildcard")g.collected.push("★");
            else{g.collected.push(f.letter);if(STICKY_SET.has(f.letter)){g.stickyUntil=now+4000;g.stickyLetter=f.letter;if(g.boostActiveUntil<=0)g.speed=g.baseSpeed*1.8;}if(f.type==="poison")g.poisonLetters.push({letter:f.letter,deadline:now+15000});}
            g.score+=(SP[f.letter]||1);
            if(f.type==="normal"){let nl;const dr=g.currentEvent?.type==="drought";if(dr){do{nl=rL();}while(VOWELS.has(nl));}else nl=rL();g.foods[i]={...rP(occ(g)),letter:nl,type:"normal"};}else g.foods.splice(i,1);
            ate=true;setUi(p=>({...p,collected:[...g.collected],score:g.score,len:g.snake.length,
              scoreReached:g.score>=LEVELS[Math.min(g.level,LEVELS.length-1)].target&&g.level<LEVELS.length}));break;}}
        // Collect falling letters
        if (g.fallingLetters) {
          for (let i = g.fallingLetters.length - 1; i >= 0; i--) {
            const fl = g.fallingLetters[i];
            if (fl.x === nx && fl.y === ny) {
              g.collected.push(fl.letter);
              g.score += (SP[fl.letter] || 1);
              g.fallingLetters.splice(i, 1);
              setUi(p => ({ ...p, collected: [...g.collected], score: g.score }));
              break;
            }
          }
        }
        for(let i=g.powerups.length-1;i>=0;i--){const pu=g.powerups[i];
          if(pu.x===nx&&pu.y===ny){
            if(pu.type==="shrink"){g.snake=g.snake.slice(0,Math.max(2,g.snake.length-3));g.flash={type:"good",time:20};setUi(p=>({...p,feedback:{type:"success",msg:"Shrink! -3"},len:g.snake.length}));setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);}
            else if(pu.type==="freeze"){g.speed=g.baseSpeed*2;setTimeout(()=>{if(g.stickyUntil===0&&g.boostActiveUntil<=performance.now())g.speed=g.baseSpeed;},5000);setUi(p=>({...p,feedback:{type:"success",msg:"Freeze! 5s slow-mo"}}));setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);}
            g.powerups.splice(i,1);break;}}
        g.speedPads.forEach(sp=>{if(sp.x===nx&&sp.y===ny){g.speed=Math.max(60,g.baseSpeed*0.5);sp.active=now+3000;setTimeout(()=>{if(g.stickyUntil===0&&g.boostActiveUntil<=performance.now())g.speed=g.baseSpeed;},3000);}});
        if(!ate)g.snake.pop();setUi(p=>({...p,len:g.snake.length}));
      }
      if(g.flash&&g.flash.time>0)g.flash.time--;draw(ts);raf.current=requestAnimationFrame(tick);
    };

    const draw=ts=>{
      const g=gs.current,c=cvs.current;if(!c||!g)return;const ctx=c.getContext("2d"),now=performance.now();
      ctx.fillStyle="#08080f";ctx.fillRect(0,0,GW,GH);
      if(g.currentEvent){const t={drought:"rgba(180,60,0,0.06)",poison:"rgba(100,0,150,0.06)",wildcard:"rgba(255,215,0,0.05)",bounty:"rgba(0,200,100,0.06)",decay:"rgba(100,100,100,0.06)"};ctx.fillStyle=t[g.currentEvent.type]||"rgba(0,0,0,0)";ctx.fillRect(0,0,GW,GH);}
      ctx.fillStyle="#181828";for(let x=0;x<COLS;x++)for(let y=0;y<ROWS;y++)ctx.fillRect(x*CELL+CELL/2-1,y*CELL+CELL/2-1,2,2);
      if(g.flash&&g.flash.time>0){const a=g.flash.time/40*0.15;ctx.fillStyle=g.flash.type==="bad"?`rgba(255,40,40,${a})`:`rgba(40,255,100,${a})`;ctx.fillRect(0,0,GW,GH);}
      g.speedPads.forEach(sp=>{ctx.fillStyle=now<sp.active?"rgba(255,255,0,0.3)":"rgba(255,255,0,0.1)";ctx.fillRect(sp.x*CELL,sp.y*CELL,CELL,CELL);ctx.font="16px serif";ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText("⚡",sp.x*CELL+CELL/2,sp.y*CELL+CELL/2);});
      g.portals.forEach((pt,pi)=>{const pulse=Math.sin(ts/300+pi)*0.3+0.7;[{x:pt.x1,y:pt.y1},{x:pt.x2,y:pt.y2}].forEach(p=>{const gr=ctx.createRadialGradient(p.x*CELL+CELL/2,p.y*CELL+CELL/2,2,p.x*CELL+CELL/2,p.y*CELL+CELL/2,CELL/2);gr.addColorStop(0,`rgba(150,50,255,${pulse})`);gr.addColorStop(1,"rgba(150,50,255,0)");ctx.fillStyle=gr;ctx.beginPath();ctx.arc(p.x*CELL+CELL/2,p.y*CELL+CELL/2,CELL/2,0,Math.PI*2);ctx.fill();ctx.font="14px serif";ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText("🌀",p.x*CELL+CELL/2,p.y*CELL+CELL/2);});});
      g.walls.forEach(w=>{ctx.fillStyle="#ff3333";ctx.shadowColor="#ff3333";ctx.shadowBlur=6;ctx.beginPath();ctx.roundRect(w.x*CELL+2,w.y*CELL+2,CELL-4,CELL-4,4);ctx.fill();ctx.shadowBlur=0;});
      g.powerups.forEach(pu=>{const icons={shrink:"💊",freeze:"❄️"};const pulse=Math.sin(ts/250)*0.3+0.7;ctx.globalAlpha=pulse;ctx.font="18px serif";ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText(icons[pu.type],pu.x*CELL+CELL/2,pu.y*CELL+CELL/2);ctx.globalAlpha=1;});
      if(g.lifelineOnGrid){const lf=g.lifelineOnGrid,pulse=Math.sin(ts/200)*0.4+0.6;const gr=ctx.createRadialGradient(lf.x*CELL+CELL/2,lf.y*CELL+CELL/2,2,lf.x*CELL+CELL/2,lf.y*CELL+CELL/2,CELL);gr.addColorStop(0,`rgba(0,255,100,${pulse*0.5})`);gr.addColorStop(1,"rgba(0,255,100,0)");ctx.fillStyle=gr;ctx.beginPath();ctx.arc(lf.x*CELL+CELL/2,lf.y*CELL+CELL/2,CELL,0,Math.PI*2);ctx.fill();ctx.font="20px serif";ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText("💚",lf.x*CELL+CELL/2,lf.y*CELL+CELL/2);}

      // === DRAW FALLING LETTERS ===
      if (g.fallingLetters) {
        g.fallingLetters.forEach(fl => {
          const cx = fl.x * CELL + CELL / 2, cy = fl.y * CELL + CELL / 2;
          const alpha = fl.landed ? Math.max(0.3, 1 - (now - fl.landTime) / 4000) : 0.9;
          // Trail
          if (!fl.landed) {
            for (let t = 1; t <= 3; t++) {
              const ty = fl.y - t;
              if (ty >= 0) {
                ctx.fillStyle = `rgba(255,120,50,${0.12 / t})`;
                ctx.fillRect(fl.x * CELL + 4, ty * CELL + 4, CELL - 8, CELL - 8);
              }
            }
          }
          ctx.globalAlpha = alpha;
          ctx.fillStyle = fl.landed ? "#1a1000" : "#2a1500";
          ctx.strokeStyle = "#ff8833";
          ctx.lineWidth = 2;
          ctx.beginPath(); ctx.roundRect(fl.x * CELL + 3, fl.y * CELL + 3, CELL - 6, CELL - 6, 4); ctx.fill(); ctx.stroke();
          ctx.fillStyle = "#ff9944";
          ctx.font = `bold 16px ${FM}`; ctx.textAlign = "center"; ctx.textBaseline = "middle";
          ctx.fillText(fl.letter, cx, cy + 1);
          ctx.fillStyle = "#664422"; ctx.font = `8px ${FM}`;
          ctx.fillText(SP[fl.letter] || "", fl.x * CELL + CELL - 7, fl.y * CELL + CELL - 5);
          ctx.globalAlpha = 1;
        });
      }

      // === DRAW FOODS ===
      g.foods.forEach(f=>{
        const isV=VOWELS.has(f.letter),isS=STICKY_SET.has(f.letter),isW=f.type==="wildcard",isP=f.type==="poison";
        const cx=f.x*CELL+CELL/2,cy=f.y*CELL+CELL/2;
        const gr=ctx.createRadialGradient(cx,cy,2,cx,cy,CELL*0.8);
        gr.addColorStop(0,isW?"rgba(255,215,0,0.6)":isP?"rgba(150,0,200,0.5)":isS?"rgba(255,50,200,0.5)":isV?"rgba(255,180,50,0.4)":"rgba(50,200,255,0.4)");gr.addColorStop(1,"rgba(0,0,0,0)");ctx.fillStyle=gr;ctx.fillRect(f.x*CELL-4,f.y*CELL-4,CELL+8,CELL+8);
        ctx.fillStyle=isW?"#2a2000":isP?"#1a002a":isS?"#1a0020":isV?"#2a1a00":"#001a2a";
        ctx.strokeStyle=isW?"#ffd700":isP?"#aa00ff":isS?"#ff44cc":isV?"#ffb832":"#32d4ff";ctx.lineWidth=2;
        ctx.beginPath();ctx.roundRect(f.x*CELL+3,f.y*CELL+3,CELL-6,CELL-6,4);ctx.fill();ctx.stroke();
        ctx.fillStyle=isW?"#ffd700":isP?"#cc66ff":isS?"#ff66dd":isV?"#ffcc44":"#44ddff";
        ctx.font=`bold 16px ${FM}`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText(f.letter,cx,cy+1);
        if(!isW){ctx.fillStyle="#555";ctx.font=`8px ${FM}`;ctx.fillText(SP[f.letter]||"",f.x*CELL+CELL-7,f.y*CELL+CELL-5);}
      });

      // === DRAW AI SNAKE ===
      if (g.aiSnake && g.aiSnake.length > 0) {
        g.aiSnake.forEach((s, i) => {
          const p = 2;
          if (i === 0) {
            ctx.fillStyle = "#ff6622"; ctx.shadowColor = "#ff6622"; ctx.shadowBlur = 8;
            ctx.beginPath(); ctx.roundRect(s.x * CELL + p, s.y * CELL + p, CELL - p * 2, CELL - p * 2, 6); ctx.fill();
            ctx.shadowBlur = 0;
            ctx.fillStyle = "#000";
            const ex = s.x * CELL + CELL / 2, ey = s.y * CELL + CELL / 2;
            ctx.fillRect(ex - 4, ey - 4, 3, 3); ctx.fillRect(ex + 1, ey - 4, 3, 3);
            // small "AI" label
            ctx.fillStyle = "rgba(255,102,34,0.7)"; ctx.font = `bold 8px ${FM}`;
            ctx.textAlign = "center"; ctx.textBaseline = "middle";
            ctx.fillText("AI", ex, s.y * CELL + CELL - 4);
          } else {
            const t = 1 - i / (g.aiSnake.length + 5);
            const rv = Math.floor(180 + 60 * t), gv = Math.floor(40 + 50 * t), bv = Math.floor(10 + 20 * t);
            ctx.fillStyle = `rgb(${rv},${gv},${bv})`;
            ctx.beginPath(); ctx.roundRect(s.x * CELL + p + 1, s.y * CELL + p + 1, CELL - p * 2 - 2, CELL - p * 2 - 2, 4); ctx.fill();
          }
        });
      }

      // === DRAW PLAYER SNAKE ===
      const isSlow=g.stickyUntil>0;
      const isBoosted = g.boostActiveUntil > 0 && now < g.boostActiveUntil;
      g.snake.forEach((s,i)=>{const p=2;
        if(i===0){
          ctx.fillStyle=isBoosted?"#44ffff":isSlow?"#cc66ff":"#4eff4e";
          ctx.shadowColor=isBoosted?"#44ffff":isSlow?"#cc66ff":"#4eff4e";
          ctx.shadowBlur=isBoosted?14:10;
          ctx.beginPath();ctx.roundRect(s.x*CELL+p,s.y*CELL+p,CELL-p*2,CELL-p*2,6);ctx.fill();ctx.shadowBlur=0;
          ctx.fillStyle="#000";const ex=s.x*CELL+CELL/2,ey=s.y*CELL+CELL/2;
          if(g.dir===DIR.RIGHT||g.dir===DIR.LEFT){ctx.fillRect(ex-3,ey-6,4,4);ctx.fillRect(ex-3,ey+2,4,4);}
          else{ctx.fillRect(ex-6,ey-3,4,4);ctx.fillRect(ex+2,ey-3,4,4);}
        }else{const t=1-i/(g.snake.length+5);let rv,gv,bv;
          if(isBoosted){rv=Math.floor(20+40*t);gv=Math.floor(200+50*t);bv=Math.floor(200+50*t);}
          else if(isSlow){rv=Math.floor(140+60*t);gv=Math.floor(60+40*t);bv=Math.floor(180+60*t);}
          else{rv=Math.floor(20+40*t);gv=Math.floor(180+60*t);bv=Math.floor(20+40*t);}
          ctx.fillStyle=`rgb(${rv},${gv},${bv})`;ctx.beginPath();ctx.roundRect(s.x*CELL+p+1,s.y*CELL+p+1,CELL-p*2-2,CELL-p*2-2,4);ctx.fill();}
      });

      // === HUD ON CANVAS ===
      if(g.currentEvent){
        const labels={drought:"DROUGHT",poison:"POISON",wildcard:"WILDCARD",bounty:"BOUNTY",decay:"DECAY"};
        const colors={drought:"rgba(180,40,0,0.9)",poison:"rgba(100,0,160,0.9)",wildcard:"rgba(160,130,0,0.9)",bounty:"rgba(0,120,70,0.9)",decay:"rgba(70,70,70,0.9)"};
        const rem=Math.max(0,Math.ceil((g.currentEvent.end-now)/1000));let label=labels[g.currentEvent.type]+` ${rem}s`;if(g.bountyWord)label+=` "${g.bountyWord.toUpperCase()}"`;
        ctx.fillStyle=colors[g.currentEvent.type];const tw=Math.max(180,label.length*9+16);ctx.beginPath();ctx.roundRect(GW/2-tw/2,6,tw,28,8);ctx.fill();
        ctx.fillStyle="#fff";ctx.font=`600 14px ${FM}`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText(label,GW/2,20);
      }

      // Level timer on canvas
      const tleft = Math.max(0, LEVEL_DURATION - (now - g.levelStartTime) / 1000);
      const tmins = Math.floor(tleft / 60), tsecs = Math.floor(tleft % 60);
      const tstr = `${tmins}:${tsecs.toString().padStart(2, "0")}`;
      ctx.fillStyle = tleft < 30 ? "rgba(255,40,40,0.9)" : tleft < 60 ? "rgba(255,170,0,0.9)" : "rgba(100,100,100,0.7)";
      ctx.beginPath(); ctx.roundRect(GW - 72, 6, 66, 24, 6); ctx.fill();
      ctx.fillStyle = "#fff"; ctx.font = `700 14px ${FM}`; ctx.textAlign = "center"; ctx.textBaseline = "middle";
      ctx.fillText(tstr, GW - 39, 18);

      if(g.gameOver){
        ctx.fillStyle="rgba(0,0,0,0.85)";ctx.fillRect(0,0,GW,GH);
        ctx.fillStyle="#ff4444";ctx.font=`800 48px ${F}`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText("GAME OVER",GW/2,GH/2-54);
        ctx.fillStyle="#ff8888";ctx.font=`500 18px ${F}`;
        const reason = g.gameOver === true ? "" : "";
        ctx.fillText(reason,GW/2,GH/2-16);
        ctx.fillStyle="#ddd";ctx.font=`600 22px ${F}`;ctx.fillText(`Score: ${g.score}  |  Level ${g.level+1}  |  ${g.uniqueWords.size} unique words`,GW/2,GH/2+14);
        ctx.fillStyle="#777";ctx.font=`500 16px ${F}`;ctx.fillText("Press Enter to restart",GW/2,GH/2+52);
      }
      if(g.wordMode){ctx.fillStyle="rgba(0,0,0,0.6)";ctx.fillRect(0,0,GW,GH);ctx.fillStyle="#ffcc44";ctx.font=`800 28px ${F}`;ctx.textAlign="center";ctx.fillText("WORD MODE",GW/2,GH/2-12);ctx.fillStyle="#888";ctx.font=`500 16px ${F}`;ctx.fillText("Press Esc to cancel",GW/2,GH/2+26);}
    };
    raf.current=requestAnimationFrame(tick);return()=>{run=false;cancelAnimationFrame(raf.current);};
  },[ui.started,ui.wordMode,tryLevelAdvance]);

  const stats = useMemo(()=>{
    const wf=ui.wordsFormed;if(!wf.length)return null;
    const longest=wf.reduce((a,b)=>b.word.length>a.word.length?b:a);
    const shortest=wf.reduce((a,b)=>b.word.length<a.word.length?b:a);
    const best=wf.reduce((a,b)=>b.score>a.score?b:a);
    return{total:wf.length,longest,shortest,best};
  },[ui.wordsFormed]);

  if(loading){
    return(
      <div style={{display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",height:"100vh",background:"#08080f",color:"#eee",fontFamily:F}}>
        <h1 style={{fontSize:56,fontWeight:900,marginBottom:20,letterSpacing:"-2px",background:"linear-gradient(135deg,#4eff4e,#44ddff,#ff44cc,#ffd700)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent"}}>SNABBLE</h1>
        <div style={{color:"#ffcc44",fontSize:20,fontWeight:500,marginBottom:12}}>{loadStatus}</div>
        <div style={{color:"#555",fontSize:15,fontWeight:400}}>Loading word lists...</div>
      </div>);
  }

  if(!ui.started){
    return(
      <div style={{display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",height:"100vh",background:"#08080f",color:"#eee",fontFamily:F,padding:20}}>
        <h1 style={{fontSize:60,fontWeight:900,marginBottom:6,letterSpacing:"-3px",background:"linear-gradient(135deg,#4eff4e,#44ddff,#ff44cc,#ffd700)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent"}}>SNABBLE</h1>
        <p style={{color:"#777",fontSize:19,fontWeight:400,marginBottom:24,letterSpacing:"0.5px"}}>Eat letters. Form words. Survive the chaos.</p>
        <div style={{color:"#4eff4e",fontSize:15,fontWeight:500,marginBottom:20,opacity:0.8}}>{loadStatus}</div>
        <div style={{background:"rgba(255,255,255,0.03)",border:"1px solid rgba(255,255,255,0.08)",borderRadius:16,padding:"28px 32px",maxWidth:620,marginBottom:32,lineHeight:2,fontSize:15,color:"#bbb",fontWeight:400,backdropFilter:"blur(10px)"}}>
          <div style={{display:"grid",gridTemplateColumns:"1fr 1fr",gap:"18px 32px"}}>
            <div>
              <div style={{color:"#fff",fontWeight:700,marginBottom:8,fontSize:18,letterSpacing:"-0.5px"}}>Controls</div>
              <div><span style={{color:"#4eff4e",fontFamily:FM,fontWeight:600}}>Arrow Keys</span> <span style={{color:"#999"}}>Move</span></div>
              <div><span style={{color:"#ffcc44",fontFamily:FM,fontWeight:600}}>W</span> <span style={{color:"#999"}}>Word Mode</span> <span style={{color:"#666",fontSize:13}}>(10s)</span></div>
              <div><span style={{color:"#44ffff",fontFamily:FM,fontWeight:600}}>B</span> <span style={{color:"#999"}}>Use Boost</span> <span style={{color:"#666",fontSize:13}}>(stored)</span></div>
            </div>
            <div>
              <div style={{color:"#fff",fontWeight:700,marginBottom:8,fontSize:18,letterSpacing:"-0.5px"}}>Level Rules</div>
              <div><span style={{color:"#4eff4e"}}>5:00</span> <span style={{color:"#999"}}>per level</span></div>
              <div><span style={{color:"#ffcc44"}}>Score</span> <span style={{color:"#999"}}>+ shed ALL letters to advance</span></div>
              <div><span style={{color:"#ff5555"}}>Fail</span> <span style={{color:"#999"}}>= +unused to length</span></div>
            </div>
          </div>
          <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",paddingTop:14,marginTop:16,fontSize:14,color:"#888",lineHeight:1.8}}>
            <span style={{color:"#ffcc44"}}>Vowel</span> <span style={{color:"#44ddff",marginLeft:8}}>Consonant</span> <span style={{color:"#ff66dd",marginLeft:8}}>Sticky</span> <span style={{color:"#ffd700",marginLeft:8}}>Wildcard</span> <span style={{marginLeft:8,color:"#4eff4e"}}>Lifeline</span> <span style={{color:"#44ffff",marginLeft:8}}>Boost</span>
          </div>
          <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",paddingTop:14,marginTop:14,fontSize:14,color:"#888",lineHeight:1.9}}>
            <span style={{color:"#aaa",fontWeight:600}}>Events</span> — Drought | Wildcard | Poison | Bounty | Decay<br/>
            <span style={{color:"#aaa",fontWeight:600}}>Items</span> — Shrink | Freeze | Speed | Portal | Wall<br/>
            <span style={{color:"#ff6622",fontWeight:600}}>Lv.5+</span> — AI snake hunts your vowels<br/>
            <span style={{color:"#ff8833",fontWeight:600}}>Lv.6+</span> — Letters fall from above (tetris-style)<br/>
            <span style={{color:"#aaa",fontWeight:600}}>8 levels</span> | combo chain words within 15s for multipliers
          </div>
        </div>
        <button onClick={()=>initGame(0)} style={{background:"linear-gradient(135deg,#4eff4e,#22bb44)",color:"#000",border:"none",padding:"18px 56px",borderRadius:14,fontSize:24,fontFamily:F,fontWeight:800,cursor:"pointer",letterSpacing:"-0.5px",boxShadow:"0 0 40px rgba(78,255,78,0.2)",transition:"transform 0.15s, box-shadow 0.15s"}} onMouseOver={e=>{e.target.style.transform="scale(1.04)";e.target.style.boxShadow="0 0 60px rgba(78,255,78,0.35)";}} onMouseOut={e=>{e.target.style.transform="scale(1)";e.target.style.boxShadow="0 0 40px rgba(78,255,78,0.2)";}}>Start Game</button>
      </div>);
  }

  const progressPct=Math.min(100,(ui.score/ui.targetScore)*100);
  const tmins=Math.floor(ui.levelTimeLeft/60), tsecs=ui.levelTimeLeft%60;
  const timeStr=`${tmins}:${tsecs.toString().padStart(2,"0")}`;

  return(
    <div style={{display:"flex",flexDirection:"column",alignItems:"center",background:"#08080f",minHeight:"100vh",fontFamily:F,color:"#eee",padding:"12px"}}>
      {/* Stats bar */}
      <div style={{display:"flex",gap:20,marginBottom:10,fontSize:17,flexWrap:"wrap",justifyContent:"center",alignItems:"center",fontWeight:500}}>
        <span style={{color:"#ffd700",fontWeight:800,fontSize:20,fontFamily:FM}}>Lv.{ui.level}</span>
        <span style={{color:ui.levelTimeLeft<=30?"#ff4444":ui.levelTimeLeft<=60?"#ffaa44":"#aaa",fontWeight:800,fontSize:20,fontFamily:FM}}>{timeStr}</span>
        <span style={{color:"#aaa"}}>Score <b style={{color:"#ffcc44",fontSize:22,fontWeight:800,fontFamily:FM,marginLeft:4}}>{ui.score.toLocaleString()}</b></span>
        <span style={{color:"#aaa"}}>Length <b style={{color:ui.len>20?"#ff4444":ui.len>12?"#ffaa44":"#4eff4e",fontSize:22,fontWeight:800,fontFamily:FM,marginLeft:4}}>{ui.len}</b></span>
        <span style={{color:"#aaa"}}>Words <b style={{color:"#44ddff",fontSize:18,fontWeight:800,fontFamily:FM,marginLeft:4}}>{ui.uniqueWordCount}</b></span>
        {ui.boosts>0&&<span style={{color:"#44ffff",fontWeight:700,fontSize:15}}>Boost x{ui.boosts} <span style={{color:"#666",fontSize:12}}>[B]</span></span>}
        {ui.boostActive&&<span style={{color:"#44ffff",fontWeight:700,fontSize:15,animation:"none"}}>BOOST ACTIVE</span>}
        {ui.combo>1&&<span style={{color:"#ffd700",fontWeight:700}}>x{ui.combo}</span>}
        {ui.stickyTimer>0&&<span style={{color:"#ff66dd",fontWeight:600,fontSize:15}}>Sticky {ui.stickyLetter} ({ui.stickyTimer}s)</span>}
        {ui.poisonTimers.map((p,i)=><span key={i} style={{color:"#cc66ff",fontWeight:600,fontSize:15}}>Poison {p.letter} ({p.secs}s)</span>)}
        {ui.scoreReached&&ui.collected.length>0&&<span style={{color:"#ff8833",fontWeight:700,fontSize:14}}>SHED ALL LETTERS TO ADVANCE</span>}
      </div>
      {/* Progress bar */}
      <div style={{width:GW+200,height:6,background:"rgba(255,255,255,0.04)",borderRadius:3,marginBottom:10,overflow:"hidden"}}>
        <div style={{height:"100%",width:`${progressPct}%`,background:ui.scoreReached?"linear-gradient(90deg,#ffd700,#ff8833)":"linear-gradient(90deg,#4eff4e,#ffd700)",borderRadius:3,transition:"width 0.3s"}}/>
      </div>

      {/* Main layout */}
      <div style={{display:"flex",gap:14,alignItems:"flex-start"}}>
        <div style={{position:"relative"}}>
          <canvas ref={cvs} width={GW} height={GH} style={{border:"1px solid rgba(255,255,255,0.06)",borderRadius:10,display:"block"}}/>
          {ui.feedback&&(
            <div style={{position:"absolute",bottom:14,left:"50%",transform:"translateX(-50%)",
              padding:"10px 24px",borderRadius:10,fontSize:16,fontWeight:600,textAlign:"center",whiteSpace:"nowrap",zIndex:10,
              fontFamily:F,letterSpacing:"-0.3px",
              background:ui.feedback.type==="success"||ui.feedback.type==="bounty"?"rgba(10,42,10,0.95)":ui.feedback.type==="penalty"?"rgba(42,10,10,0.95)":"rgba(42,26,0,0.95)",
              border:`2px solid ${ui.feedback.type==="success"?"#4eff4e":ui.feedback.type==="bounty"?"#ffd700":ui.feedback.type==="penalty"?"#ff4444":"#ffaa00"}`,
              color:ui.feedback.type==="success"?"#4eff4e":ui.feedback.type==="bounty"?"#ffd700":ui.feedback.type==="penalty"?"#ff4444":"#ffaa00",
              boxShadow:`0 4px 24px ${ui.feedback.type==="success"||ui.feedback.type==="bounty"?"rgba(78,255,78,0.15)":ui.feedback.type==="penalty"?"rgba(255,68,68,0.15)":"rgba(255,170,0,0.15)"}`
            }}>{ui.feedback.msg}</div>
          )}
        </div>

        {/* Side panel */}
        <div style={{width:190,background:"rgba(255,255,255,0.02)",border:"1px solid rgba(255,255,255,0.06)",borderRadius:12,padding:"14px 12px",display:"flex",flexDirection:"column",gap:0,backdropFilter:"blur(10px)"}}>
          <div style={{color:"#777",fontSize:13,marginBottom:10,textAlign:"center",fontWeight:700,letterSpacing:"1.5px",textTransform:"uppercase"}}>Collected <span style={{color:"#aaa",fontFamily:FM}}>({ui.collected.length})</span></div>
          <div style={{display:"flex",gap:5,flexWrap:"wrap",justifyContent:"center",minHeight:44}}>
            {ui.collected.map((l,i)=>{
              const isV=VOWELS.has(l),isS=STICKY_SET.has(l),isW=l==="★",isP=ui.poisonTimers.some(p=>p.letter===l);
              return(<span key={i} style={{display:"inline-flex",alignItems:"center",justifyContent:"center",position:"relative",
                width:36,height:40,borderRadius:6,fontSize:21,fontWeight:700,fontFamily:FM,
                background:isW?"#2a2000":isP?"#1a002a":isS?"#1a0020":isV?"#2a1a00":"#001a2a",
                border:`2px solid ${isW?"#ffd700":isP?"#aa00ff":isS?"#ff44cc":isV?"#ffb832":"#32d4ff"}`,
                color:isW?"#ffd700":isP?"#cc66ff":isS?"#ff66dd":isV?"#ffcc44":"#44ddff",
                boxShadow:`0 0 8px ${isW?"rgba(255,215,0,0.2)":isP?"rgba(170,0,255,0.15)":isS?"rgba(255,68,204,0.15)":isV?"rgba(255,184,50,0.1)":"rgba(50,212,255,0.1)"}`
              }}>{l}{!isW&&<span style={{position:"absolute",bottom:1,right:3,fontSize:9,color:"#555",fontWeight:500}}>{SP[l]}</span>}</span>);
            })}
            {ui.collected.length===0&&<span style={{color:"#444",fontSize:14,padding:"12px 0",textAlign:"center",fontWeight:400,lineHeight:1.5}}>Eat letters<br/>to collect</span>}
          </div>

          <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",marginTop:12,paddingTop:10,textAlign:"center"}}>
            <div style={{color:"#777",fontSize:13,fontWeight:500}}>{"Need >= "}<b style={{color:ui.collected.length>=Math.ceil(ui.len/2)?"#4eff4e":"#ff5555",fontFamily:FM,fontWeight:700}}>{Math.ceil(ui.len/2)}</b>{" to shed safely"}</div>
            <div style={{color:"#555",fontSize:13,marginTop:2,fontWeight:400}}>Fail = <span style={{color:"#ff6666",fontWeight:600}}>+unused to length</span></div>
            {ui.scoreReached&&<div style={{color:"#ff8833",fontSize:12,marginTop:4,fontWeight:700}}>Shed all to advance!</div>}
            <div style={{color:"#555",fontSize:13,marginTop:6,fontWeight:400}}>Press <span style={{color:"#ffcc44",fontWeight:700,fontFamily:FM}}>W</span> for Word Mode</div>
            {ui.boosts>0&&<div style={{color:"#44ffff",fontSize:13,marginTop:4,fontWeight:600}}>Press <span style={{fontFamily:FM,fontWeight:700}}>B</span> for Boost ({ui.boosts})</div>}
          </div>

          {ui.hasAi&&(
            <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",marginTop:12,paddingTop:8,textAlign:"center"}}>
              <div style={{color:"#ff6622",fontSize:12,fontWeight:700,letterSpacing:"0.5px"}}>AI SNAKE ACTIVE</div>
              <div style={{color:"#884422",fontSize:11,fontWeight:400,marginTop:2}}>Hunting your vowels</div>
            </div>
          )}
          {ui.hasFalling&&(
            <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",marginTop:8,paddingTop:8,textAlign:"center"}}>
              <div style={{color:"#ff8833",fontSize:12,fontWeight:700,letterSpacing:"0.5px"}}>LETTERS FALLING</div>
              <div style={{color:"#885522",fontSize:11,fontWeight:400,marginTop:2}}>Catch them as they drop</div>
            </div>
          )}

          {stats&&(
            <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",marginTop:12,paddingTop:10}}>
              <div style={{color:"#777",fontSize:11,textAlign:"center",marginBottom:8,fontWeight:700,letterSpacing:"1.5px",textTransform:"uppercase"}}>Stats</div>
              <div style={{display:"grid",gridTemplateColumns:"1fr 1fr",gap:6}}>
                <div style={{background:"rgba(78,255,78,0.04)",border:"1px solid rgba(78,255,78,0.1)",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#666",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>TOTAL</div>
                  <div style={{color:"#4eff4e",fontSize:26,fontWeight:800,fontFamily:FM}}>{stats.total}</div>
                </div>
                <div style={{background:"rgba(68,221,255,0.04)",border:"1px solid rgba(68,221,255,0.1)",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#666",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>UNIQUE</div>
                  <div style={{color:"#44ddff",fontSize:26,fontWeight:800,fontFamily:FM}}>{ui.uniqueWordCount}</div>
                </div>
                <div style={{background:"rgba(255,215,0,0.04)",border:"1px solid rgba(255,215,0,0.1)",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#666",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>BEST</div>
                  <div style={{color:"#ffd700",fontSize:14,fontWeight:700,fontFamily:FM}}>{stats.best.word}</div>
                  <div style={{color:"#886600",fontSize:10,fontWeight:500}}>+{stats.best.score}</div>
                </div>
                <div style={{background:"rgba(204,102,255,0.04)",border:"1px solid rgba(204,102,255,0.1)",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#666",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>LONGEST</div>
                  <div style={{color:"#cc66ff",fontSize:14,fontWeight:700,fontFamily:FM}}>{stats.longest.word}</div>
                  <div style={{color:"#663388",fontSize:10,fontWeight:500}}>{stats.longest.word.length} letters</div>
                </div>
              </div>
            </div>
          )}

          {ui.wordsFormed.length>0&&(
            <div style={{borderTop:"1px solid rgba(255,255,255,0.06)",marginTop:12,paddingTop:10}}>
              <div style={{color:"#777",fontSize:11,textAlign:"center",marginBottom:6,fontWeight:700,letterSpacing:"1.5px",textTransform:"uppercase"}}>Recent</div>
              <div style={{display:"flex",flexDirection:"column",gap:4}}>
                {ui.wordsFormed.slice(-5).reverse().map((w,i)=>(
                  <div key={i} style={{fontSize:12,padding:"4px 8px",borderRadius:6,textAlign:"center",fontWeight:600,fontFamily:FM,letterSpacing:"0.5px",
                    background:w.bounty?"rgba(255,215,0,0.06)":w.penalty?"rgba(255,68,68,0.06)":"rgba(78,255,78,0.06)",
                    border:`1px solid ${w.bounty?"rgba(255,215,0,0.2)":w.penalty?"rgba(255,68,68,0.2)":"rgba(78,255,78,0.2)"}`,
                    color:w.bounty?"#ffd700":w.penalty?"#ff5555":"#4eff4e"
                  }}>{w.word}{w.combo>1?` x${w.combo}`:""}{w.bounty?" BOUNTY":""} +{w.score}</div>
                ))}
              </div>
            </div>
          )}
        </div>
      </div>

      {/* Word Mode input */}
      {ui.wordMode&&(
        <div style={{marginTop:14,display:"flex",gap:12,alignItems:"center"}}>
          <div style={{width:56,height:56,borderRadius:"50%",display:"flex",alignItems:"center",justifyContent:"center",
            fontSize:26,fontWeight:800,fontFamily:FM,
            background:ui.wordTimeLeft<=3?"rgba(255,68,68,0.1)":"rgba(255,204,68,0.08)",
            border:`3px solid ${ui.wordTimeLeft<=3?"#ff4444":"#ffcc44"}`,
            color:ui.wordTimeLeft<=3?"#ff4444":"#ffcc44",
            boxShadow:`0 0 20px ${ui.wordTimeLeft<=3?"rgba(255,68,68,0.2)":"rgba(255,204,68,0.15)"}`
          }}>{ui.wordTimeLeft}</div>
          <input autoFocus value={ui.wordInput} onChange={e=>setUi(p=>({...p,wordInput:e.target.value.toUpperCase()}))}
            onKeyDown={e=>{if(e.key==="Enter")submitWord(ui.wordInput);if(e.key==="Escape")exitWordMode();}}
            placeholder="Type a word..."
            style={{background:"rgba(255,255,255,0.03)",border:`2px solid ${ui.wordTimeLeft<=3?"#ff4444":"#ffcc44"}`,color:"#ffcc44",padding:"12px 18px",borderRadius:10,fontSize:22,fontFamily:FM,fontWeight:600,width:240,outline:"none",letterSpacing:"1px"}}/>
          <button onClick={()=>submitWord(ui.wordInput)}
            style={{background:"#ffcc44",color:"#000",border:"none",padding:"12px 24px",borderRadius:10,fontSize:17,fontFamily:F,fontWeight:700,cursor:"pointer",letterSpacing:"-0.3px"}}>Submit</button>
          <button onClick={exitWordMode}
            style={{background:"rgba(255,255,255,0.06)",color:"#888",border:"1px solid rgba(255,255,255,0.1)",padding:"12px 18px",borderRadius:10,fontSize:15,fontFamily:F,fontWeight:500,cursor:"pointer"}}>Esc</button>
        </div>
      )}
    </div>
  );
}
