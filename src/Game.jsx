import { useState, useEffect, useRef, useCallback, useMemo } from "react";
import { COLS, ROWS, CELL, GW, GH, VOWELS, STICKY_SET, LW, SP, DIR, DIRS_ARR,
  LEVELS, WORD_TIMER, LEVEL_DURATION, AI_START_LEVEL, FALLING_START_LEVEL,
  BOOST_INTERVAL, BOOST_DURATION, BOOST_SPEED_MULT, PITFALL_START_LEVEL,
  PITFALL_SPAWN_INTERVAL, PITFALL_MORPH_TIME, PITFALL_TYPES, FALLING_SPAWN_INTERVAL,
  FALLING_SPEED, PORTAL_REPOSITION_INTERVAL, EVENT_TYPES, EVENT_DUR
} from "./constants.js";

let _nonVowelRun = 0;
const rL = ()=>{
  if(_nonVowelRun>=4){_nonVowelRun=0;const v="AEIOU";return v[Math.floor(Math.random()*v.length)];}
  const ch=LW[Math.floor(Math.random()*LW.length)];
  if(VOWELS.has(ch))_nonVowelRun=0;else _nonVowelRun++;
  return ch;
};
const rP = (avoid)=>{let p,t=0;do{p={x:Math.floor(Math.random()*COLS),y:Math.floor(Math.random()*ROWS)};t++;}while(t<500&&avoid.some(a=>a.x===p.x&&a.y===p.y));return p;};
const occ = g=>[...g.snake,...g.foods,...g.walls,...g.powerups,...g.speedPads,
  ...g.portals.flatMap(p=>[{x:p.x1,y:p.y1},{x:p.x2,y:p.y2}]),
  ...(g.aiSnake||[]),...(g.fallingLetters||[]),...(g.pitfalls||[])];
const DICT_URL = "https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt";

const F = "'Fredoka', system-ui, sans-serif";
const FM = "'Fredoka', system-ui, sans-serif";
const FN = "'Fredoka', system-ui, monospace";

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
  const wordInputRef = useRef(null);
  const [loading, setLoading] = useState(true);
  const [loadStatus, setLoadStatus] = useState("Fetching dictionary...");
  const [ui, setUi] = useState({
    started:false,gameOver:false,wordMode:false,wordInput:"",feedback:null,wordError:null,
    score:0,len:2,collected:[],wordsFormed:[],level:1,targetScore:300,
    event:null,eventTimer:0,combo:0,stickyTimer:0,stickyLetter:"",
    poisonTimers:[],checking:false,wordTimeLeft:WORD_TIMER,transitioning:false,
    levelTimeLeft:LEVEL_DURATION,uniqueWordCount:0,boosts:0,boostActive:false,
    scoreReached:false,hasAi:false,hasFalling:false,hasPitfalls:false,gameOverReason:"",
    totalLettersEaten:0,totalElapsed:0,transitionCountdown:0,totalDistance:0
  });

  useEffect(()=>{
    const link = document.createElement('link');
    link.href = 'https://fonts.googleapis.com/css2?family=Fredoka:wght@300;400;500;600;700&display=swap';
    link.rel = 'stylesheet';
    document.head.appendChild(link);
    return () => document.head.removeChild(link);
  }, []);

  useEffect(()=>{
    let cancelled = false;
    (async()=>{
      const words = new Set();
      try {
        setLoadStatus("Fetching dictionary...");
        const res = await fetch(DICT_URL, {signal: AbortSignal.timeout(20000)}).catch(()=>null);
        if(res && res.ok){
          const txt = await res.text();
          txt.split(/\r?\n/).forEach(w=>{
            const lw = w.trim().toLowerCase();
            if(lw.length>=3 && lw.length<=10 && /^[a-z]+$/.test(lw)) words.add(lw);
          });
        }
      } catch(e) { console.warn("Dict fetch failed:", e); }
      if(!cancelled){
        if(words.size > 1000){
          dictRef.current = words;
          setLoadStatus(`${words.size.toLocaleString()} words loaded`);
        } else {
          dictRef.current = new Set("ace,act,add,age,ago,aid,aim,air,all,and,ant,any,ape,arc,are,ark,arm,art,ash,ask,ate,awe,axe,bad,bag,ban,bar,bat,bay,bed,bee,bet,bid,big,bit,bow,box,boy,bud,bug,bun,bus,but,buy,cab,can,cap,car,cat,cop,cow,cry,cub,cup,cut,dad,dam,day,den,dew,did,die,dig,dim,dip,doc,doe,dog,don,dot,dry,dub,dud,due,dug,duo,dye,ear,eat,eel,egg,ego,elm,emu,end,era,eve,ewe,eye,fan,far,fat,fax,fed,fee,few,fig,fin,fir,fit,fix,fly,foe,fog,fox,fry,fun,fur,gag,gap,gas,gel,gem,get,gin,god,got,gum,gun,gut,guy,gym,had,ham,has,hat,hay,hen,her,hew,hex,hid,him,hip,his,hit,hoe,hog,hop,hot,how,hub,hue,hug,hum,hut,ice,icy,ill,imp,ink,inn,ion,ire,irk,ivy,jab,jam,jar,jaw,jay,jet,jig,job,jog,jot,joy,jug,jut,keg,key,kid,kin,kit,lab,lad,lag,lap,law,lay,led,leg,let,lid,lie,lip,lit,log,lot,low,lug,mad,man,map,mar,mat,max,may,men,met,mid,mix,mob,mod,mom,mop,mow,mud,mug,mum,nab,nag,nap,net,new,nil,nip,nit,nod,nor,not,now,nun,nut,oak,oar,oat,odd,ode,off,oil,old,one,opt,orb,ore,our,out,owe,owl,own,pad,pal,pan,par,pat,paw,pay,pea,peg,pen,per,pet,pew,pie,pig,pin,pit,ply,pod,pop,pot,pro,pry,pub,pun,pup,put,rag,ram,ran,rap,rat,raw,ray,red,ref,rib,rid,rig,rim,rip,rob,rod,rot,row,rub,rug,rum,run,rut,rye,sad,sag,sap,sat,saw,say,sea,set,sew,she,shy,sin,sip,sir,sit,six,ski,sky,sly,sob,son,sow,soy,spa,spy,sub,sue,sum,sun,tab,tag,tan,tap,tar,tax,tea,ten,the,tic,tie,tin,tip,toe,ton,too,top,tot,tow,toy,try,tub,tug,two,urn,use,van,vat,vet,via,vie,vow,wad,wag,war,was,wax,way,web,wed,wet,who,why,wig,win,wit,woe,wok,won,woo,wow,yak,yam,yap,yea,yes,yet,yew,you,zap,zen,zip,zoo".split(","));
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
    const prevSnake = keepScore && gs.current ? gs.current.snake.map(s=>({...s})) : null;
    const prevCollected = keepScore && gs.current ? [...gs.current.collected] : [];
    const snake = prevSnake || (()=>{
      const arr=[];const sx=Math.floor(COLS/2),sy=Math.floor(ROWS/2);
      for(let i=0;i<2;i++)arr.push({x:sx-i,y:sy});return arr;
    })();
    const L=LEVELS[Math.min(lvl,LEVELS.length-1)];
    const foods=[];for(let i=0;i<4;i++)foods.push({...rP([...snake,...foods]),letter:rL(),type:"normal"});
    const all=[...snake,...foods];
    const walls=[];for(let i=0;i<L.walls;i++){const w=rP(all);walls.push({...w,dx:Math.random()<0.5?1:-1,dy:0});all.push(w);}
    const speedPads=[];for(let i=0;i<L.pads;i++){const s=rP(all);speedPads.push({...s,active:0});all.push(s);}
    const portals=[];for(let i=0;i<L.portals;i++){const p1=rP(all);all.push(p1);const p2=rP(all);all.push(p2);portals.push({x1:p1.x,y1:p1.y,x2:p2.x,y2:p2.y});}
    const ps=keepScore&&gs.current?gs.current.score:0;
    const pw=keepScore&&gs.current?gs.current.wordsFormed:[];
    const puw=keepScore&&gs.current?gs.current.uniqueWords:new Set();
    const pboosts=keepScore&&gs.current?gs.current.boosts:0;
    const pTotalLetters=keepScore&&gs.current?gs.current.totalLettersEaten:0;
    const pGameStart=keepScore&&gs.current?gs.current.gameStartTime:performance.now();
    const pTotalDist=keepScore&&gs.current?gs.current.totalDistance:0;
    const lifelineCooldown = 18000 + lvl * 6000 + Math.random() * (10000 + lvl * 5000);
    const now = performance.now();
    const hasAi = lvl >= AI_START_LEVEL;
    const hasFalling = lvl >= FALLING_START_LEVEL;
    const hasPitfalls = lvl >= PITFALL_START_LEVEL;
    let aiSnake = [], aiDir = DIR.LEFT, aiSpeed = Math.max(200, 350 - lvl * 20);
    if (hasAi) {
      const ax = Math.floor(COLS * 0.8), ay = Math.floor(ROWS * 0.2);
      aiSnake = [{ x: ax, y: ay }, { x: ax + 1, y: ay }];
    }
    gs.current={snake,dir:DIR.RIGHT,nextDir:DIR.RIGHT,foods,collected:prevCollected,score:ps,
      gameOver:false,wordMode:false,baseSpeed:L.spd,speed:L.spd,lastMove:0,flash:null,
      wordsFormed:[...pw],uniqueWords:puw,gameStart:now,level:lvl,levelStartTime:now,
      currentEvent:null,lastEventTime:now,eventCooldown:40000+Math.random()*20000,
      stickyUntil:0,stickyLetter:"",walls,speedPads,portals,powerups:[],powerupTimer:now+20000,
      comboCount:0,lastWordTime:0,poisonLetters:[],wildcardActive:false,
      wallMoveInterval:Math.max(400,800-lvl*60),wallLastMove:0,
      lifelineTimer:now+lifelineCooldown,lifelineOnGrid:null,
      aiSnake,aiDir,aiSpeed,aiLastMove:now,
      fallingLetters:[],lastFallingSpawn:now,
      pitfalls:[],lastPitfallSpawn:now,
      boosts:pboosts,boostActiveUntil:0,lastBoostGrant:now,
      portalMoveTimer:now,
      levelAdvancePending:false,
      totalLettersEaten:pTotalLetters,
      gameStartTime:pGameStart,
      totalDistance:pTotalDist};
    setUi({started:true,gameOver:false,wordMode:false,wordInput:"",feedback:null,
      score:ps,len:snake.length,collected:prevCollected,level:lvl+1,targetScore:L.target,
      event:null,eventTimer:0,combo:0,stickyTimer:0,stickyLetter:"",
      poisonTimers:[],checking:false,wordTimeLeft:WORD_TIMER,
      wordsFormed:[...pw],transitioning:false,
      levelTimeLeft:LEVEL_DURATION,uniqueWordCount:puw.size,boosts:pboosts,boostActive:false,
      scoreReached:false,hasAi,hasFalling,hasPitfalls,gameOverReason:"",
      totalLettersEaten:pTotalLetters,totalElapsed:0,transitionCountdown:0,totalDistance:pTotalDist});
  },[]);

  const hasLetters=(word,coll)=>{
    const pool=[...coll];let wc=pool.filter(c=>c==="★").length;
    for(const ch of word.toUpperCase()){const i=pool.indexOf(ch);if(i!==-1)pool.splice(i,1);else if(wc>0){wc--;const wi=pool.indexOf("★");if(wi!==-1)pool.splice(wi,1);}else return false;}return true;};

  const exitWordMode=useCallback(()=>{
    if(wtRef.current){clearInterval(wtRef.current);wtRef.current=null;}
    if(gs.current)gs.current.wordMode=false;
    setUi(p=>({...p,wordMode:false,wordInput:"",feedback:null,wordError:null,wordTimeLeft:WORD_TIMER}));
  },[]);

  const onWordTimeout=useCallback(()=>{
    if(wtRef.current){clearInterval(wtRef.current);wtRef.current=null;}
    const g=gs.current;if(!g)return;g.wordMode=false;
    const penalty = g.collected.length;
    const newLen = g.snake.length + penalty;
    const tail=g.snake[g.snake.length-1];while(g.snake.length<newLen)g.snake.push({...tail});
    g.flash={type:"bad",time:40};
    setUi(p=>({...p,wordMode:false,wordInput:"",wordTimeLeft:WORD_TIMER,len:g.snake.length,
      feedback:{type:"grow",delta:penalty}}));
    setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);
  },[]);

  const tryLevelAdvance = useCallback((g) => {
    if (!g || g.levelAdvancePending) return;
    const lvlIdx = g.level;
    const nextTarget = LEVELS[Math.min(lvlIdx, LEVELS.length - 1)].target;
    if (g.score >= nextTarget && lvlIdx < LEVELS.length) {
      g.levelAdvancePending = true;
      setTimeout(() => initGame(lvlIdx + 1, true), 800);
    }
  }, [initGame]);

  const submitWord=useCallback((word)=>{
    const g=gs.current;if(!g)return;
    const w=word.toLowerCase().trim();
    if(w.length<2){setUi(p=>({...p,wordError:"Minimum 2 letters"}));return;}
    if(!hasLetters(w,g.collected)){setUi(p=>({...p,wordError:"You don't have those letters"}));return;}
    if(!isValidWord(w)){setUi(p=>({...p,wordError:`"${w.toUpperCase()}" not in dictionary`}));return;}
    if(wtRef.current){clearInterval(wtRef.current);wtRef.current=null;}

    const used=[...w.toUpperCase()];const rem=[...g.collected];
    for(const ch of used){let i=rem.indexOf(ch);if(i===-1)i=rem.indexOf("★");if(i!==-1)rem.splice(i,1);}
    const unusedCount = rem.length;
    g.collected=rem;
    if(g.stickyUntil>0&&used.includes(g.stickyLetter)){g.stickyUntil=0;g.speed=g.baseSpeed;}

    g.uniqueWords.add(w);

    const shed=w.length,snakeLen=g.snake.length,halfLen=Math.ceil(snakeLen/2);
    let scrPts=0;for(const ch of w.toUpperCase())scrPts+=(SP[ch]||1);
    const now=performance.now();
    if(g.lastWordTime>0&&now-g.lastWordTime<15000)g.comboCount++;else g.comboCount=1;
    g.lastWordTime=now;const comboMult=Math.min(g.comboCount,5);
    let newLen,penalty=false;
    const perfectShed = rem.length === 0;
    if(shed>=halfLen){
      newLen=Math.max(2,snakeLen-shed);
      const basePts = scrPts*10+(shed>=5?200:0);
      g.score+=(perfectShed ? basePts*2 : basePts)*comboMult;
      g.flash={type:"good",time:30};
    }else{
      newLen=snakeLen+unusedCount;
      penalty=true;g.score+=scrPts*2*comboMult;g.flash={type:"bad",time:40};
    }
    if(newLen<g.snake.length)g.snake=g.snake.slice(0,newLen);
    else{const tail=g.snake[g.snake.length-1];while(g.snake.length<newLen)g.snake.push({...tail});}
    const basePtsCalc = scrPts*10+(shed>=5?200:0);
    const pts=penalty?scrPts*2*comboMult:(perfectShed ? basePtsCalc*2 : basePtsCalc)*comboMult;
    g.wordsFormed.push({word:w.toUpperCase(),shed,penalty,score:pts,combo:comboMult});
    g.wordMode=false;

    const lvlIdx=g.level;const nextTarget=LEVELS[Math.min(lvlIdx,LEVELS.length-1)].target;
    const scoreReached = g.score >= nextTarget && lvlIdx < LEVELS.length;

    const delta = Math.abs(newLen - snakeLen);
    const fb = penalty ? {type:"grow",delta} : delta > 0 ? {type:"shrink",delta} : null;

    setUi(p=>({...p,wordMode:false,wordInput:"",collected:[...g.collected],score:g.score,len:g.snake.length,
      wordsFormed:[...g.wordsFormed],combo:g.comboCount,wordTimeLeft:WORD_TIMER,
      uniqueWordCount:g.uniqueWords.size,scoreReached,
      feedback:fb}));

    if (scoreReached) {
      tryLevelAdvance(g);
    } else if (fb) {
      setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);
    }
  },[initGame,isValidWord,tryLevelAdvance]);

  // Auto-focus word input when word mode opens
  useEffect(() => {
    if (ui.wordMode && wordInputRef.current) {
      setTimeout(() => wordInputRef.current.focus(), 50);
    }
  }, [ui.wordMode]);

  const boostTimerRef = useRef(null);
  useEffect(()=>{
    const h=e=>{const g=gs.current;if(!g||g.gameOver){if(e.key===" "&&g?.gameOver){e.preventDefault();initGame(0);}return;}
      if(g.wordMode){if(e.key==="Escape")exitWordMode();return;}
      if(e.key==="w"||e.key==="W"){
        if(g.collected.length<2){g.banner={msg:"Need at least 2 letters",time:performance.now()+2000};return;}
        e.preventDefault();g.wordMode=true;
        setUi(p=>({...p,wordMode:true,wordInput:"",feedback:null,wordError:null,wordTimeLeft:WORD_TIMER}));
        if(wtRef.current)clearInterval(wtRef.current);
        let tl=WORD_TIMER;
        wtRef.current=setInterval(()=>{tl--;setUi(p=>({...p,wordTimeLeft:tl}));if(tl<=0)onWordTimeout();},1000);
        return;
      }
      const km={ArrowUp:"UP",ArrowDown:"DOWN",ArrowLeft:"LEFT",ArrowRight:"RIGHT"};
      const d=km[e.key];if(d){
        e.preventDefault();const nd=DIR[d];if(nd.x+g.dir.x!==0||nd.y+g.dir.y!==0)g.nextDir=nd;
        // Long-press up arrow = boost
        if(d==="UP"&&!boostTimerRef.current){
          boostTimerRef.current=setTimeout(()=>{
            const g2=gs.current;if(g2&&g2.boosts>0&&g2.boostActiveUntil<=performance.now()){
              g2.boosts--;g2.boostActiveUntil=performance.now()+BOOST_DURATION;
              g2.speed=Math.max(60,g2.baseSpeed*BOOST_SPEED_MULT);
              setUi(p=>({...p,boosts:g2.boosts,boostActive:true}));
            }
            boostTimerRef.current=null;
          },300);
        }
      }
    };
    const hu=e=>{
      if(e.key==="ArrowUp"&&boostTimerRef.current){clearTimeout(boostTimerRef.current);boostTimerRef.current=null;}
    };
    window.addEventListener("keydown",h);window.addEventListener("keyup",hu);
    return()=>{window.removeEventListener("keydown",h);window.removeEventListener("keyup",hu);if(boostTimerRef.current)clearTimeout(boostTimerRef.current);};
  },[initGame,exitWordMode,onWordTimeout]);

  // Touch controls for mobile
  useEffect(()=>{
    const el=cvs.current;if(!el)return;
    let tx=0,ty=0,longT=null;
    const ts2=e=>{e.preventDefault();const t=e.touches[0];tx=t.clientX;ty=t.clientY;
      longT=setTimeout(()=>{
        const g=gs.current;if(g&&g.boosts>0&&g.boostActiveUntil<=performance.now()){
          g.boosts--;g.boostActiveUntil=performance.now()+BOOST_DURATION;
          g.speed=Math.max(60,g.baseSpeed*BOOST_SPEED_MULT);
          setUi(p=>({...p,boosts:g.boosts,boostActive:true}));
        }longT=null;
      },500);
    };
    const te=e=>{
      if(longT){clearTimeout(longT);longT=null;}
      const t=e.changedTouches[0];const dx=t.clientX-tx,dy=t.clientY-ty;
      const g=gs.current;if(!g)return;
      if(g.gameOver){initGame(0);return;}
      const dist=Math.sqrt(dx*dx+dy*dy);
      if(dist<20){
        // Tap — open word mode
        if(!g.wordMode&&g.collected.length>=2){
          g.wordMode=true;
          setUi(p=>({...p,wordMode:true,wordInput:"",feedback:null,wordError:null,wordTimeLeft:WORD_TIMER}));
          if(wtRef.current)clearInterval(wtRef.current);
          let tl=WORD_TIMER;
          wtRef.current=setInterval(()=>{tl--;setUi(p=>({...p,wordTimeLeft:tl}));if(tl<=0)onWordTimeout();},1000);
        } else if(!g.wordMode&&g.collected.length<2){
          g.banner={msg:"Need at least 2 letters",time:performance.now()+2000};
        }
        return;
      }
      if(g.wordMode)return;
      // Swipe direction
      let nd;
      if(Math.abs(dx)>Math.abs(dy)){nd=dx>0?DIR.RIGHT:DIR.LEFT;}
      else{nd=dy>0?DIR.DOWN:DIR.UP;}
      if(nd.x+g.dir.x!==0||nd.y+g.dir.y!==0)g.nextDir=nd;
    };
    const tm=e=>{e.preventDefault();};
    const tc=()=>{if(longT){clearTimeout(longT);longT=null;}};
    el.addEventListener("touchstart",ts2,{passive:false});
    el.addEventListener("touchmove",tm,{passive:false});
    el.addEventListener("touchend",te);
    el.addEventListener("touchcancel",tc);
    return()=>{el.removeEventListener("touchstart",ts2);el.removeEventListener("touchmove",tm);el.removeEventListener("touchend",te);el.removeEventListener("touchcancel",tc);};
  },[initGame,onWordTimeout]);

  useEffect(()=>{
    if(!gs.current)return;let run=true;
    const tick=ts=>{
      if(!run)return;const g=gs.current;
      if(!g||g.gameOver||g.wordMode){draw(ts);raf.current=requestAnimationFrame(tick);return;}
      const now=performance.now();

      // === LEVEL TIMER ===
      const levelElapsed = (now - g.levelStartTime) / 1000;
      const levelTimeLeft = Math.max(0, LEVEL_DURATION - levelElapsed);
      const totalElapsed = Math.floor((now - g.gameStartTime) / 1000);
      setUi(p=>({...p,levelTimeLeft:Math.ceil(levelTimeLeft),totalElapsed,totalDistance:g.totalDistance}));
      if (levelTimeLeft <= 0) {
        g.gameOver = true;g.gameOverTime=now;
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
        setUi(p => ({ ...p, boosts: g.boosts }));
      }

      // === EVENTS ===
      if(!g.currentEvent&&now-g.lastEventTime>=g.eventCooldown){
        const et=EVENT_TYPES[Math.floor(Math.random()*EVENT_TYPES.length)];
        g.currentEvent={type:et,start:now,end:now+EVENT_DUR[et]};g.lastEventTime=now;g.eventCooldown=45000+Math.random()*25000;
        if(et==="wildcard"){g.foods.push({...rP(occ(g)),letter:"★",type:"wildcard"});}
        setUi(p=>({...p,event:et,eventTimer:Math.ceil(EVENT_DUR[et]/1000)}));
      }
      if(g.currentEvent){
        if(now>=g.currentEvent.end){
          if(g.currentEvent.type==="wildcard"){
            g.foods=g.foods.filter(f=>f.type!=="wildcard");
            // Remove unused wildcards from collected and apply penalty
            const wcCount=g.collected.filter(c=>c==="★").length;
            if(wcCount>0){
              g.collected=g.collected.filter(c=>c!=="★");
              const penalty=wcCount*3;
              const tail=g.snake[g.snake.length-1];
              for(let i=0;i<penalty;i++)g.snake.push({...tail});
              g.flash={type:"bad",time:40};
              setUi(p=>({...p,collected:[...g.collected],len:g.snake.length,
                feedback:{type:"grow",delta:penalty}}));
              setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);
            }
          }
          g.currentEvent=null;setUi(p=>({...p,event:null,eventTimer:0}));
        }else{
          setUi(p=>({...p,eventTimer:Math.max(1,Math.ceil((g.currentEvent.end-now)/1000))}));
        }
      }
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

      // === PITFALLS ===
      if (g.level >= PITFALL_START_LEVEL) {
        if (now - g.lastPitfallSpawn >= PITFALL_SPAWN_INTERVAL) {
          g.lastPitfallSpawn = now;
          const ptype = PITFALL_TYPES[Math.floor(Math.random() * PITFALL_TYPES.length)];
          const pos = rP(occ(g));
          g.pitfalls.push({ x: pos.x, y: pos.y, type: ptype, spawn: now, morphed: false,
            letter: ptype === "letter" ? rL() : null });
        }
        g.pitfalls.forEach(pf => {
          if (!pf.morphed && now - pf.spawn >= PITFALL_MORPH_TIME) {
            pf.morphed = true;
            const others = PITFALL_TYPES.filter(t => t !== pf.type);
            pf.type = others[Math.floor(Math.random() * others.length)];
            if (pf.type === "letter") pf.letter = rL(); else pf.letter = null;
          }
        });
        g.pitfalls = g.pitfalls.filter(pf => now - pf.spawn < 8000);
      }

      // === PLAYER MOVEMENT ===
      if(ts-g.lastMove>=g.speed){
        g.lastMove=ts;g.dir=g.nextDir;
        const head=g.snake[0];let nx=(head.x+g.dir.x+COLS)%COLS,ny=(head.y+g.dir.y+ROWS)%ROWS;
        for(const pt of g.portals){if(nx===pt.x1&&ny===pt.y1){nx=pt.x2;ny=pt.y2;break;}if(nx===pt.x2&&ny===pt.y2){nx=pt.x1;ny=pt.y1;break;}}
        if(g.walls.some(w=>w.x===nx&&w.y===ny)){g.gameOver=true;g.gameOverTime=now;setUi(p=>({...p,gameOver:true,gameOverReason:"Hit a wall!"}));draw(ts);raf.current=requestAnimationFrame(tick);return;}
        if(g.snake.some((s,i)=>i>0&&s.x===nx&&s.y===ny)){g.gameOver=true;g.gameOverTime=now;setUi(p=>({...p,gameOver:true,gameOverReason:"Hit yourself!"}));draw(ts);raf.current=requestAnimationFrame(tick);return;}
        g.snake.unshift({x:nx,y:ny});
        g.totalDistance++;
        if(g.lifelineOnGrid&&g.lifelineOnGrid.x===nx&&g.lifelineOnGrid.y===ny){
          const sh=Math.max(3,Math.floor(g.snake.length*0.4));g.snake=g.snake.slice(0,Math.max(2,g.snake.length-sh));g.lifelineOnGrid=null;g.flash={type:"good",time:25};
          setUi(p=>({...p,len:g.snake.length,feedback:{type:"shrink",delta:sh}}));setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);}
        let ate=false;
        for(let i=0;i<g.foods.length;i++){const f=g.foods[i];
          if(f.x===nx&&f.y===ny){
            if(f.type==="wildcard")g.collected.push("★");
            else{g.collected.push(f.letter);if(STICKY_SET.has(f.letter)){g.stickyUntil=now+4000;g.stickyLetter=f.letter;if(g.boostActiveUntil<=0)g.speed=g.baseSpeed*1.8;}}
            g.score+=(SP[f.letter]||1);g.totalLettersEaten++;
            if(f.type==="normal"){g.foods[i]={...rP(occ(g)),letter:rL(),type:"normal"};}else g.foods.splice(i,1);
            ate=true;setUi(p=>({...p,collected:[...g.collected],score:g.score,len:g.snake.length,totalLettersEaten:g.totalLettersEaten,
              scoreReached:g.score>=LEVELS[Math.min(g.level,LEVELS.length-1)].target&&g.level<LEVELS.length}));break;}}
        // Collect falling letters
        if (g.fallingLetters) {
          for (let i = g.fallingLetters.length - 1; i >= 0; i--) {
            const fl = g.fallingLetters[i];
            if (fl.x === nx && fl.y === ny) {
              g.collected.push(fl.letter);
              g.score += (SP[fl.letter] || 1);
              g.totalLettersEaten++;
              g.fallingLetters.splice(i, 1);
              setUi(p => ({ ...p, collected: [...g.collected], score: g.score }));
              break;
            }
          }
        }
        for(let i=g.powerups.length-1;i>=0;i--){const pu=g.powerups[i];
          if(pu.x===nx&&pu.y===ny){
            if(pu.type==="shrink"){const pre=g.snake.length;g.snake=g.snake.slice(0,Math.max(2,g.snake.length-3));g.flash={type:"good",time:20};const d=pre-g.snake.length;setUi(p=>({...p,feedback:d>0?{type:"shrink",delta:d}:null,len:g.snake.length}));if(d>0)setTimeout(()=>setUi(p=>({...p,feedback:null})),2000);}
            else if(pu.type==="freeze"){g.speed=g.baseSpeed*2;setTimeout(()=>{if(g.stickyUntil===0&&g.boostActiveUntil<=performance.now())g.speed=g.baseSpeed;},5000);}
            g.powerups.splice(i,1);break;}}
        g.speedPads.forEach(sp=>{if(sp.x===nx&&sp.y===ny){g.speed=Math.max(60,g.baseSpeed*0.5);sp.active=now+3000;setTimeout(()=>{if(g.stickyUntil===0&&g.boostActiveUntil<=performance.now())g.speed=g.baseSpeed;},3000);}});
        // === PITFALL COLLISION ===
        if (g.pitfalls) {
          for (let i = g.pitfalls.length - 1; i >= 0; i--) {
            const pf = g.pitfalls[i];
            if (pf.x === nx && pf.y === ny) {
              if (pf.type === "speed") {
                g.speed = g.baseSpeed * 2;
                g.flash = { type: "bad", time: 30 };
                setTimeout(() => { if (g.stickyUntil === 0 && g.boostActiveUntil <= performance.now()) g.speed = g.baseSpeed; }, 4000);
              } else if (pf.type === "bomb") {
                const add = 4 + Math.floor(g.level * 0.5);
                const tail = g.snake[g.snake.length - 1];
                for (let j = 0; j < add; j++) g.snake.push({ ...tail });
                g.flash = { type: "bad", time: 40 };
                setUi(p => ({ ...p, len: g.snake.length, feedback: {type:"grow",delta:add} }));
                setTimeout(() => setUi(p => ({ ...p, feedback: null })), 2000);
              } else if (pf.type === "letter" && pf.letter) {
                g.collected.push(pf.letter);
                g.score += (SP[pf.letter] || 1);
                g.totalLettersEaten++;
                g.flash = { type: "good", time: 20 };
                setUi(p => ({ ...p, collected: [...g.collected], score: g.score }));
              }
              g.pitfalls.splice(i, 1);
              break;
            }
          }
        }
        if(!ate)g.snake.pop();setUi(p=>({...p,len:g.snake.length}));
      }
      if(g.flash&&g.flash.time>0)g.flash.time--;draw(ts);raf.current=requestAnimationFrame(tick);
    };

    // === DESERT DAWN THEME COLORS ===
    const BG = "#f5f0ea";
    const GRID_LINE = "#EDE3D4";
    const TILE_GREEN = "#D4A574";
    const TILE_VOWEL = "#C67B5C";
    const TILE_STICKY = "#3E2723";
    const TILE_WILD = "#E8B84E";
    const TILE_LETTER = "#fff";
    const SNAKE_HEAD = "#b5623e";
    const SNAKE_BODY_H = 25;

    const drawTile = (ctx, x, y, letter, bg, fg, val) => {
      const tx = x * CELL, ty = y * CELL, cx = tx + CELL / 2;
      ctx.fillStyle = bg;
      ctx.beginPath(); ctx.roundRect(tx + 2, ty + 2, CELL - 4, CELL - 4, 7); ctx.fill();
      ctx.fillStyle = fg;
      ctx.font = `bold 26px ${FM}`; ctx.textAlign = "center"; ctx.textBaseline = "middle";
      ctx.fillText(letter, cx, ty + CELL / 2 - 1);
      if (val !== undefined) {
        ctx.fillStyle = fg; ctx.globalAlpha = 0.7;
        ctx.font = `bold 13px ${FN}`; ctx.textAlign = "right"; ctx.textBaseline = "bottom";
        ctx.fillText(val, tx + CELL - 5, ty + CELL - 3);
        ctx.globalAlpha = 1;
      }
    };

    const draw=ts=>{
      const g=gs.current,c=cvs.current;if(!c||!g)return;const ctx=c.getContext("2d"),now=performance.now();
      ctx.fillStyle=BG;ctx.fillRect(0,0,GW,GH);
      // draw empty cells with rounded inset look
      for(let cx=0;cx<COLS;cx++){for(let cy=0;cy<ROWS;cy++){
        ctx.fillStyle="#ebe3d8";ctx.beginPath();ctx.roundRect(cx*CELL+2,cy*CELL+2,CELL-4,CELL-4,7);ctx.fill();
      }}
      // event tint
      if(g.currentEvent){ctx.fillStyle="rgba(232,184,78,0.1)";ctx.fillRect(0,0,GW,GH);}
      // flash
      if(g.flash&&g.flash.time>0){const a=g.flash.time/40*0.12;ctx.fillStyle=g.flash.type==="bad"?`rgba(180,50,50,${a})`:`rgba(212,165,116,${a})`;ctx.fillRect(0,0,GW,GH);}

      // speed pads
      g.speedPads.forEach(sp=>{
        ctx.fillStyle=now<sp.active?"#E8B84E":"#d4a842";ctx.beginPath();ctx.roundRect(sp.x*CELL+2,sp.y*CELL+2,CELL-4,CELL-4,7);ctx.fill();
        ctx.fillStyle="#3E2723";ctx.font=`bold 24px serif`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText("\u26A1",sp.x*CELL+CELL/2,sp.y*CELL+CELL/2);
      });
      // portals
      g.portals.forEach((pt,pi)=>{const pulse=Math.sin(ts/300+pi)*0.15+0.85;[{x:pt.x1,y:pt.y1},{x:pt.x2,y:pt.y2}].forEach(p=>{
        ctx.globalAlpha=pulse;
        ctx.fillStyle="#D4A574";ctx.beginPath();ctx.roundRect(p.x*CELL+2,p.y*CELL+2,CELL-4,CELL-4,7);ctx.fill();
        ctx.strokeStyle="#C67B5C";ctx.lineWidth=2;ctx.beginPath();ctx.arc(p.x*CELL+CELL/2,p.y*CELL+CELL/2,CELL/3,0,Math.PI*2);ctx.stroke();
        ctx.fillStyle="#C67B5C";ctx.beginPath();ctx.arc(p.x*CELL+CELL/2,p.y*CELL+CELL/2,5,0,Math.PI*2);ctx.fill();
        ctx.globalAlpha=1;
      });});
      // walls
      g.walls.forEach(w=>{ctx.fillStyle="#3E2723";ctx.beginPath();ctx.roundRect(w.x*CELL+2,w.y*CELL+2,CELL-4,CELL-4,7);ctx.fill();});
      // powerups
      g.powerups.forEach(pu=>{const pulse=Math.sin(ts/250)*0.3+0.7;ctx.globalAlpha=pulse;
        ctx.fillStyle=pu.type==="shrink"?"#D4A574":"#E8B84E";ctx.beginPath();ctx.roundRect(pu.x*CELL+2,pu.y*CELL+2,CELL-4,CELL-4,7);ctx.fill();
        ctx.fillStyle="#3E2723";ctx.font=`bold 24px serif`;ctx.textAlign="center";ctx.textBaseline="middle";
        ctx.fillText(pu.type==="shrink"?"\uD83D\uDC8A":"\u26A1",pu.x*CELL+CELL/2,pu.y*CELL+CELL/2);ctx.globalAlpha=1;});
      // lifeline
      if(g.lifelineOnGrid){const lf=g.lifelineOnGrid,pulse=Math.sin(ts/200)*0.3+0.7;ctx.globalAlpha=pulse;
        ctx.fillStyle="#4CAF50";ctx.beginPath();ctx.roundRect(lf.x*CELL+2,lf.y*CELL+2,CELL-4,CELL-4,7);ctx.fill();
        ctx.fillStyle="#fff";ctx.font=`bold 26px serif`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText("\u2764",lf.x*CELL+CELL/2,lf.y*CELL+CELL/2);ctx.globalAlpha=1;}

      // === DRAW FALLING LETTERS ===
      if (g.fallingLetters) {
        g.fallingLetters.forEach(fl => {
          const alpha = fl.landed ? Math.max(0.3, 1 - (now - fl.landTime) / 4000) : 0.9;
          if (!fl.landed) { for (let t = 1; t <= 2; t++) { const ty = fl.y - t; if (ty >= 0) { ctx.fillStyle = `rgba(198,123,92,${0.08/t})`; ctx.fillRect(fl.x*CELL+3,ty*CELL+3,CELL-6,CELL-6); }}}
          ctx.globalAlpha = alpha;
          drawTile(ctx, fl.x, fl.y, fl.letter, "#5D4037", TILE_LETTER, SP[fl.letter]);
          ctx.globalAlpha = 1;
        });
      }

      // === DRAW PITFALLS ===
      if (g.pitfalls) {
        g.pitfalls.forEach(pf => {
          const cx = pf.x * CELL + CELL / 2, cy = pf.y * CELL + CELL / 2;
          const age = now - pf.spawn;
          const pulse = Math.sin(ts / 200) * 0.2 + 0.8;
          const fade = age > 6000 ? Math.max(0.3, 1 - (age - 6000) / 2000) : 1;
          ctx.globalAlpha = fade * pulse;
          if (pf.type === "speed") {
            drawTile(ctx, pf.x, pf.y, "\uD83D\uDC0C", "#5D4037", "", undefined);
          } else if (pf.type === "bomb") {
            drawTile(ctx, pf.x, pf.y, "\uD83D\uDCA3", "#3E2723", "", undefined);
          } else if (pf.type === "letter" && pf.letter) {
            drawTile(ctx, pf.x, pf.y, pf.letter, "#E8B84E", "#3E2723", SP[pf.letter]);
          }
          if (!pf.morphed && age > PITFALL_MORPH_TIME * 0.7) {
            const warn = Math.sin(ts / 100) > 0;
            if (warn) { ctx.strokeStyle = "#c0392b"; ctx.lineWidth = 2; ctx.beginPath(); ctx.roundRect(pf.x*CELL+1,pf.y*CELL+1,CELL-2,CELL-2,6); ctx.stroke(); }
          }
          ctx.globalAlpha = 1;
        });
      }

      // === DRAW FOODS (periodic table style) ===
      g.foods.forEach(f=>{
        const isV=VOWELS.has(f.letter),isS=STICKY_SET.has(f.letter),isW=f.type==="wildcard";
        const bg = isW ? TILE_WILD : isS ? TILE_STICKY : isV ? TILE_VOWEL : TILE_GREEN;
        drawTile(ctx, f.x, f.y, f.letter, bg, TILE_LETTER, isW ? undefined : SP[f.letter]);
      });

      // === DRAW AI SNAKE ===

      if (g.aiSnake && g.aiSnake.length > 0) {
        g.aiSnake.forEach((s, i) => {
          const p = 2;
          if (i === 0) {
            ctx.fillStyle = "#b33030"; ctx.beginPath(); ctx.roundRect(s.x*CELL+p,s.y*CELL+p,CELL-p*2,CELL-p*2,6); ctx.fill();
            ctx.fillStyle = "#fff"; ctx.font = `bold 20px ${FM}`; ctx.textAlign = "center"; ctx.textBaseline = "middle";
            ctx.fillText(":-x", s.x*CELL+CELL/2, s.y*CELL+CELL/2);
          } else {
            const t = 1 - i / (g.aiSnake.length + 5);
            ctx.fillStyle = `rgb(${Math.floor(150+40*t)},${Math.floor(35+20*t)},${Math.floor(35+15*t)})`;
            ctx.beginPath(); ctx.roundRect(s.x*CELL+p+1,s.y*CELL+p+1,CELL-p*2-2,CELL-p*2-2,4); ctx.fill();
          }
        });
      }

      // === DRAW PLAYER SNAKE ===
      const isSlow=g.stickyUntil>0;
      const isBoosted = g.boostActiveUntil > 0 && now < g.boostActiveUntil;
      g.snake.forEach((s,i)=>{const p=2;
        if(i===0){
          ctx.fillStyle=isBoosted?"#E8B84E":isSlow?"#3E2723":SNAKE_HEAD;
          ctx.beginPath();ctx.roundRect(s.x*CELL+p,s.y*CELL+p,CELL-p*2,CELL-p*2,7);ctx.fill();
          ctx.fillStyle="#fff";const ex=s.x*CELL+CELL/2,ey=s.y*CELL+CELL/2;
          if(g.dir===DIR.RIGHT||g.dir===DIR.LEFT){ctx.fillRect(ex-3,ey-6,4,4);ctx.fillRect(ex-3,ey+2,4,4);}
          else{ctx.fillRect(ex-6,ey-3,4,4);ctx.fillRect(ex+2,ey-3,4,4);}
        }else{const t=1-i/(g.snake.length+5);let rv,gv,bv;
          if(isBoosted){rv=Math.floor(200+32*t);gv=Math.floor(160+24*t);bv=Math.floor(50+28*t);}
          else if(isSlow){rv=Math.floor(50+12*t);gv=Math.floor(30+8*t);bv=Math.floor(25+10*t);}
          else{rv=Math.floor(170+28*t);gv=Math.floor(100+23*t);bv=Math.floor(70+22*t);}
          ctx.fillStyle=`rgb(${rv},${gv},${bv})`;ctx.beginPath();ctx.roundRect(s.x*CELL+p+1,s.y*CELL+p+1,CELL-p*2-2,CELL-p*2-2,4);ctx.fill();}
      });

      // === HUD ON CANVAS ===
      // Banner messages
      if(g.banner&&now<g.banner.time){
        const label=g.banner.msg;
        ctx.fillStyle="rgba(93,64,55,0.92)";const tw=Math.max(200,label.length*9+24);ctx.beginPath();ctx.roundRect(GW/2-tw/2,GH-38,tw,30,8);ctx.fill();
        ctx.fillStyle="#EDE3D4";ctx.font=`600 13px ${FM}`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText(label,GW/2,GH-23);
      }
      if(g.currentEvent){
        const labels={wildcard:"WILDCARD"};
        const colors={wildcard:"rgba(232,184,78,0.9)"};
        const rem=Math.max(0,Math.ceil((g.currentEvent.end-now)/1000));let label=labels[g.currentEvent.type]+` ${rem}s`;
        ctx.fillStyle=colors[g.currentEvent.type];const tw=Math.max(180,label.length*10+16);ctx.beginPath();ctx.roundRect(GW/2-tw/2,6,tw,28,8);ctx.fill();
        ctx.fillStyle="#fff";ctx.font=`600 13px ${FM}`;ctx.textAlign="center";ctx.textBaseline="middle";ctx.fillText(label,GW/2,20);
      }


      if(g.gameOver){
        ctx.fillStyle="rgba(40,25,18,0.95)";ctx.fillRect(0,0,GW,GH);
        const cx=GW/2;let y=50;
        // Title
        ctx.fillStyle="#c0392b";ctx.font=`800 44px ${F}`;ctx.textAlign="center";ctx.textBaseline="middle";
        ctx.fillText("GAME OVER",cx,y);y+=32;
        // Reason
        const reason=g.gameOverReason||"";
        if(reason){ctx.fillStyle="#E8B84E";ctx.font=`600 16px ${F}`;ctx.fillText(reason,cx,y);}
        y+=34;
        // Stats grid
        const te=Math.floor(((g.gameOverTime||performance.now())-g.gameStartTime)/1000);
        const tm=Math.floor(te/60),tsc=te%60;
        const wf=g.wordsFormed||[];
        const uw=g.uniqueWords?g.uniqueWords.size:0;
        const best=wf.length?wf.reduce((a,b)=>b.score>a.score?b:a):null;
        const longest=wf.length?wf.reduce((a,b)=>b.word.length>a.word.length?b:a):null;
        const rows=[
          ["Score",g.score.toLocaleString()],
          ["Level",`${g.level+1}`],
          ["Words Formed",`${wf.length}`],
          ["Unique Words",`${uw}`],
          ["Letters Eaten",`${g.totalLettersEaten}`],
          ["Distance",`${g.totalDistance}`],
          ["Time",`${tm}:${tsc.toString().padStart(2,"0")}`],
        ];
        if(best)rows.push(["Best Word",`${best.word} (+${best.score})`]);
        if(longest)rows.push(["Longest Word",`${longest.word} (${longest.word.length})`]);
        const colW=260,rowH=28,startX=cx-colW/2;
        for(let i=0;i<rows.length;i++){
          const ry=y+i*rowH;
          ctx.fillStyle=i%2===0?"rgba(93,64,55,0.3)":"rgba(93,64,55,0.15)";
          ctx.fillRect(startX,ry-rowH/2+2,colW,rowH);
          ctx.fillStyle="#D4A574";ctx.font=`600 14px ${F}`;ctx.textAlign="left";ctx.textBaseline="middle";
          ctx.fillText(rows[i][0],startX+12,ry+2);
          ctx.fillStyle="#EDE3D4";ctx.font=`700 14px ${FN}`;ctx.textAlign="right";
          ctx.fillText(rows[i][1],startX+colW-12,ry+2);
        }
        y+=rows.length*rowH+24;
        // Restart prompt
        ctx.fillStyle="#EDE3D4";ctx.font=`600 18px ${F}`;ctx.textAlign="center";ctx.fillText("Restart?",cx,y);
        y+=28;
        ctx.fillStyle="#E8B84E";ctx.font=`500 15px ${F}`;ctx.fillText("Press Space to play again",cx,y);
      }
      if(g.wordMode){ctx.fillStyle="rgba(40,25,18,0.75)";ctx.fillRect(0,0,GW,GH);ctx.fillStyle="#EDE3D4";ctx.font=`800 28px ${F}`;ctx.textAlign="center";ctx.fillText("WORD MODE",GW/2,GH/2-12);ctx.fillStyle="#E8B84E";ctx.font=`500 16px ${F}`;ctx.fillText("Press Esc to cancel",GW/2,GH/2+26);}
    };
    raf.current=requestAnimationFrame(tick);return()=>{run=false;cancelAnimationFrame(raf.current);};
  },[ui.started,ui.wordMode,ui.transitionCountdown,tryLevelAdvance]);

  const stats = useMemo(()=>{
    const wf=ui.wordsFormed;if(!wf.length)return null;
    const longest=wf.reduce((a,b)=>b.word.length>a.word.length?b:a);
    const shortest=wf.reduce((a,b)=>b.word.length<a.word.length?b:a);
    const best=wf.reduce((a,b)=>b.score>a.score?b:a);
    return{total:wf.length,longest,shortest,best};
  },[ui.wordsFormed]);

  // Compute which collected letters the typed word uses, and the running score
  const wordCalc = useMemo(() => {
    const input = ui.wordInput.toUpperCase();
    const pool = [...ui.collected];
    const used = new Array(pool.length).fill(false);
    let valid = true;
    let score = 0;
    for (const ch of input) {
      let found = false;
      for (let i = 0; i < pool.length; i++) {
        if (!used[i] && pool[i] === ch) { used[i] = true; found = true; score += (SP[ch] || 1); break; }
      }
      if (!found) {
        // Try wildcard
        for (let i = 0; i < pool.length; i++) {
          if (!used[i] && pool[i] === "★") { used[i] = true; found = true; break; }
        }
      }
      if (!found) valid = false;
    }
    return { used, score, valid, len: input.length };
  }, [ui.wordInput, ui.collected]);

  // Responsive: detect if narrow screen
  const [isMobile, setIsMobile] = useState(()=>window.innerWidth<960);
  useEffect(()=>{
    const calc=()=>setIsMobile(window.innerWidth<960);
    window.addEventListener("resize",calc);
    return()=>window.removeEventListener("resize",calc);
  },[]);

  // Spacebar to start game from start screen
  useEffect(() => {
    if (ui.started || loading) return;
    const h = e => { if (e.key === " ") { e.preventDefault(); initGame(0); } };
    window.addEventListener("keydown", h);
    return () => window.removeEventListener("keydown", h);
  }, [ui.started, loading, initGame]);

  if(loading){
    return(
      <div style={{display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",height:"100vh",background:"#f5f0ea",color:"#3E2723",fontFamily:F}}>
        <h1 style={{fontSize:56,fontWeight:900,marginBottom:20,letterSpacing:"-2px",color:"#C67B5C"}}>SNABBLE</h1>
        <div style={{color:"#D4A574",fontSize:20,fontWeight:500,marginBottom:12}}>{loadStatus}</div>
        <div style={{color:"#8a7b6b",fontSize:15,fontWeight:400}}>Loading word lists...</div>
      </div>);
  }

  if(!ui.started){
    return(
      <div style={{display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",height:"100vh",background:"#f5f0ea",color:"#3E2723",fontFamily:F,padding:20}}>
        <h1 style={{fontSize:60,fontWeight:900,marginBottom:6,letterSpacing:"-3px",color:"#C67B5C"}}>SNABBLE</h1>
        <p style={{color:"#D4A574",fontSize:16,fontWeight:400,marginBottom:24,letterSpacing:"0.5px"}}>Eat letters. Form words. Survive the chaos.</p>
        <div style={{background:"#f0e8dc",border:"1px solid #EDE3D4",borderRadius:16,padding:"20px 24px",maxWidth:520,width:"90%",boxSizing:"border-box",marginBottom:32,lineHeight:2,fontSize:15,color:"#6b5a4a",fontWeight:400}}>
          <div style={{display:"grid",gridTemplateColumns:"1fr 1fr",gap:"12px 24px"}}>
            <div>
              <div style={{color:"#C67B5C",fontWeight:700,marginBottom:8,fontSize:18,letterSpacing:"-0.5px"}}>Controls</div>
              <div><span style={{color:"#C67B5C",fontFamily:FM,fontWeight:600}}>Arrow Keys / Swipe</span> <span style={{color:"#D4A574"}}>Move</span></div>
              <div><span style={{color:"#E8B84E",fontFamily:FM,fontWeight:600}}>W / Tap</span> <span style={{color:"#D4A574"}}>Word Mode</span> <span style={{color:"#8a7b6b",fontSize:13}}>(10s)</span></div>
              <div><span style={{color:"#5D4037",fontFamily:FM,fontWeight:600}}>&#8593; Hold / Long press</span> <span style={{color:"#D4A574"}}>Boost</span></div>
            </div>
            <div>
              <div style={{color:"#C67B5C",fontWeight:700,marginBottom:8,fontSize:18,letterSpacing:"-0.5px"}}>Levels</div>
              <div><span style={{color:"#C67B5C"}}>5:00</span> <span style={{color:"#D4A574"}}>per level</span></div>
              <div><span style={{color:"#E8B84E"}}>Score</span> <span style={{color:"#D4A574"}}>target to advance</span></div>
              <div><span style={{color:"#3E2723",fontWeight:600}}>8 levels</span> <span style={{color:"#D4A574"}}>total</span></div>
            </div>
          </div>
        </div>
        <div style={{display:"flex",gap:16,alignItems:"center"}}>
          <button onClick={()=>initGame(0)} style={{background:"#C67B5C",color:"#fff",border:"none",padding:"18px 56px",borderRadius:14,fontSize:24,fontFamily:F,fontWeight:800,cursor:"pointer",letterSpacing:"-0.5px",boxShadow:"0 4px 20px rgba(198,123,92,0.3)",transition:"transform 0.15s, box-shadow 0.15s"}} onMouseOver={e=>{e.target.style.transform="scale(1.04)";e.target.style.boxShadow="0 4px 30px rgba(198,123,92,0.45)";}} onMouseOut={e=>{e.target.style.transform="scale(1)";e.target.style.boxShadow="0 4px 20px rgba(198,123,92,0.3)";}}>Start</button>
        </div>
        <div style={{color:"#8a7b6b",fontSize:13,marginTop:14,fontWeight:400}}>Press <span style={{color:"#C67B5C",fontWeight:700,fontFamily:FM}}>Space</span> or tap to start</div>
      </div>);
  }

  const progressPct=Math.min(100,(ui.score/ui.targetScore)*100);

  return(
    <div style={{display:"flex",flexDirection:"column",alignItems:"center",background:"#f5f0ea",minHeight:"100vh",overflow:"auto",fontFamily:F,color:"#3E2723",padding:isMobile?"6px 4px":"12px"}}>
      {/* Stats bar — level and score in matching boxes */}
      <div style={{display:"flex",gap:12,marginBottom:10,justifyContent:"center",alignItems:"center"}}>
        <div style={{background:"#f0e8dc",border:"1px solid #EDE3D4",borderRadius:10,padding:"6px 20px",textAlign:"center",minWidth:100}}>
          <div style={{color:"#D4A574",fontSize:14,fontWeight:700,letterSpacing:"1px",textTransform:"uppercase"}}>Level</div>
          <div style={{color:"#C67B5C",fontSize:28,fontWeight:800,fontFamily:FN}}>{ui.level}</div>
        </div>
        <div style={{background:"#f0e8dc",border:"1px solid #EDE3D4",borderRadius:10,padding:"6px 20px",textAlign:"center",minWidth:140}}>
          <div style={{color:"#D4A574",fontSize:14,fontWeight:700,letterSpacing:"1px",textTransform:"uppercase"}}>Score</div>
          <div style={{color:"#C67B5C",fontSize:28,fontWeight:800,fontFamily:FN}}>{ui.score.toLocaleString()}</div>
        </div>
      </div>
      {/* Progress bar */}
      <div style={{width:"100%",maxWidth:GW+(isMobile?0:214),marginBottom:10,padding:"0 4px",boxSizing:"border-box"}}>
        <div style={{display:"flex",justifyContent:"space-between",alignItems:"center",marginBottom:4}}>
          <span style={{color:"#D4A574",fontSize:12,fontWeight:600}}>{ui.score.toLocaleString()}</span>
          <span style={{color:"#8a7b6b",fontSize:12,fontWeight:500}}>{ui.targetScore.toLocaleString()}</span>
        </div>
        <div style={{height:10,background:"#EDE3D4",borderRadius:5,overflow:"hidden"}}>
          <div style={{height:"100%",width:`${progressPct}%`,background:ui.scoreReached?"linear-gradient(90deg,#E8B84E,#C67B5C)":"linear-gradient(90deg,#C67B5C,#D4A574)",borderRadius:5,transition:"width 0.3s"}}/>
        </div>
      </div>

      {/* Main layout */}
      <div style={{display:"flex",flexDirection:isMobile?"column":"row",gap:isMobile?8:14,alignItems:isMobile?"center":"flex-start",position:"relative",width:"100%",maxWidth:GW+228}}>
        <div style={{position:"relative",flexShrink:0,width:isMobile?"100%":"auto",maxWidth:GW}}>
          <canvas ref={cvs} width={GW} height={GH} style={{border:"2px solid #EDE3D4",borderRadius:10,display:"block",width:"100%",height:"auto",touchAction:"none"}}/>
          {!ui.wordMode&&ui.feedback&&(ui.feedback.type==="grow"||ui.feedback.type==="shrink")&&(
            <div style={{position:"absolute",bottom:14,left:"50%",transform:"translateX(-50%)",
              display:"flex",alignItems:"center",gap:8,
              padding:"8px 20px",borderRadius:8,zIndex:10,
              background:ui.feedback.type==="grow"?"rgba(192,57,43,0.92)":"rgba(76,175,80,0.92)",
              boxShadow:"0 4px 16px rgba(0,0,0,0.15)"
            }}>
              <div style={{width:Math.min(120,ui.feedback.delta*16),height:10,borderRadius:5,
                background:ui.feedback.type==="grow"?"#e74c3c":"#66bb6a"}}/>
              <span style={{color:"#fff",fontSize:18,fontWeight:800,fontFamily:FN,letterSpacing:"1px"}}>
                {ui.feedback.type==="grow"?"+":"\u2212"}{ui.feedback.delta}
              </span>
            </div>
          )}
          {/* Word Mode — fixed-width anchored below canvas */}
          {ui.wordMode&&(
            <div style={{position:"absolute",top:"calc(100% + 10px)",left:"50%",transform:"translateX(-50%)",zIndex:100,
              width:"100%",boxSizing:"border-box",
              background:"rgba(62,39,35,0.95)",padding:"16px 20px",borderRadius:14,boxShadow:"0 6px 24px rgba(0,0,0,0.3)"}}>
              {/* Row 1: timer, input, score, buttons */}
              <div style={{display:"flex",alignItems:"center",gap:10}}>
                <div style={{width:44,height:44,flexShrink:0,borderRadius:"50%",display:"flex",alignItems:"center",justifyContent:"center",
                  fontSize:20,fontWeight:800,fontFamily:FN,
                  background:ui.wordTimeLeft<=3?"#fdeaea":"#f0e8dc",
                  border:`3px solid ${ui.wordTimeLeft<=3?"#c0392b":"#C67B5C"}`,
                  color:ui.wordTimeLeft<=3?"#c0392b":"#C67B5C"
                }}>{ui.wordTimeLeft}</div>
                <div style={{flex:1,minWidth:0,display:"flex",flexDirection:"column",gap:2}}>
                  <input ref={wordInputRef} value={ui.wordInput} onChange={e=>setUi(p=>({...p,wordInput:e.target.value.toUpperCase(),wordError:null}))}
                    onKeyDown={e=>{if(e.key==="Enter")submitWord(ui.wordInput);if(e.key==="Escape")exitWordMode();}}
                    placeholder="Type a word..."
                    tabIndex={0}
                    style={{background:"#fff",border:`2px solid ${ui.wordError?"#c0392b":ui.wordTimeLeft<=3?"#c0392b":"#C67B5C"}`,color:"#3E2723",padding:"8px 12px",borderRadius:10,fontSize:20,fontFamily:FM,fontWeight:600,width:"100%",boxSizing:"border-box",outline:"none",letterSpacing:"1px"}}/>
                  {ui.wordError&&<div style={{color:"#e74c3c",fontSize:11,fontWeight:600,fontFamily:F,paddingLeft:4}}>{ui.wordError}</div>}
                </div>
                <div style={{width:60,flexShrink:0,textAlign:"center",color:"#E8B84E",fontSize:18,fontWeight:800,fontFamily:FN}}>
                  {wordCalc.len>=2?`${wordCalc.score} pts`:""}
                </div>
                <button onClick={()=>submitWord(ui.wordInput)} tabIndex={0}
                  style={{flexShrink:0,background:"#C67B5C",color:"#fff",border:"none",padding:"8px 16px",borderRadius:10,fontSize:15,fontFamily:F,fontWeight:700,cursor:"pointer"}}>Submit</button>
                <button onClick={exitWordMode} tabIndex={0}
                  style={{flexShrink:0,background:"#EDE3D4",color:"#3E2723",border:"1px solid #d4c4b0",padding:"8px 12px",borderRadius:10,fontSize:13,fontFamily:F,fontWeight:500,cursor:"pointer"}}>Esc</button>
              </div>
              {/* Row 2: collected letters */}
              <div style={{display:"flex",gap:4,flexWrap:"wrap",justifyContent:"center",marginTop:10}}>
                {ui.collected.map((l,i)=>{
                  const isV=VOWELS.has(l),isS=STICKY_SET.has(l),isW=l==="★";
                  const bg=isW?"#E8B84E":isS?"#3E2723":isV?"#C67B5C":"#D4A574";
                  const isUsed = wordCalc.used[i];
                  return(<span key={i} style={{display:"inline-flex",alignItems:"center",justifyContent:"center",position:"relative",
                    width:36,height:40,borderRadius:5,fontSize:20,fontWeight:700,fontFamily:FM,
                    background:bg,color:"#fff",
                    opacity:isUsed?0.35:1,
                    outline:isUsed?"2px solid #C67B5C":"none",
                    transition:"opacity 0.15s"
                  }}>{l}{!isW&&<span style={{position:"absolute",bottom:1,right:3,fontSize:9,color:"rgba(255,255,255,0.6)",fontWeight:600}}>{SP[l]}</span>}</span>);
                })}
              </div>
              {/* Feedback bar inside word box */}
              {ui.feedback&&(ui.feedback.type==="grow"||ui.feedback.type==="shrink")&&(
                <div style={{display:"flex",alignItems:"center",justifyContent:"center",gap:8,marginTop:10,
                  padding:"6px 16px",borderRadius:8,
                  background:ui.feedback.type==="grow"?"rgba(192,57,43,0.92)":"rgba(76,175,80,0.92)"}}>
                  <div style={{width:Math.min(120,ui.feedback.delta*16),height:8,borderRadius:4,
                    background:ui.feedback.type==="grow"?"#e74c3c":"#66bb6a"}}/>
                  <span style={{color:"#fff",fontSize:16,fontWeight:800,fontFamily:FN}}>
                    {ui.feedback.type==="grow"?"+":"\u2212"}{ui.feedback.delta}
                  </span>
                </div>
              )}
            </div>
          )}
        </div>

        {/* Side panel */}
        <div style={{width:isMobile?"100%":210,background:"#f0e8dc",border:"1px solid #EDE3D4",borderRadius:12,padding:"14px 12px",display:"flex",flexDirection:"column",gap:0,boxSizing:"border-box"}}>
          {(()=>{const tl=ui.levelTimeLeft,tm=Math.floor(tl/60),ts=Math.floor(tl%60),ts2=`${tm}:${ts.toString().padStart(2,"0")}`;
            const clr=tl<=30?"#c0392b":tl<=60?"#5D4037":"#C67B5C";
            return(<div style={{display:"flex",alignItems:"center",justifyContent:"center",gap:8,height:52,marginBottom:12,borderBottom:"1px solid #EDE3D4"}}>
              <span style={{fontSize:26,lineHeight:1}}>&#9202;</span>
              <span style={{fontSize:32,fontWeight:700,fontFamily:FN,color:clr,letterSpacing:"2px",lineHeight:1}}>{ts2}</span>
            </div>);
          })()}
          <div style={{color:"#D4A574",fontSize:13,marginBottom:10,textAlign:"center",fontWeight:700,letterSpacing:"1.5px",textTransform:"uppercase"}}>Collected <span style={{color:"#3E2723",fontFamily:FN}}>({ui.collected.length})</span></div>
          <div style={{display:"flex",gap:5,flexWrap:"wrap",justifyContent:"center",minHeight:44}}>
            {ui.collected.map((l,i)=>{
              const isV=VOWELS.has(l),isS=STICKY_SET.has(l),isW=l==="★";
              const bg=isW?"#E8B84E":isS?"#3E2723":isV?"#C67B5C":"#D4A574";
              return(<span key={i} style={{display:"inline-flex",alignItems:"center",justifyContent:"center",position:"relative",
                width:42,height:46,borderRadius:6,fontSize:24,fontWeight:700,fontFamily:FM,
                background:bg,color:"#fff"
              }}>{l}{!isW&&<span style={{position:"absolute",bottom:2,right:4,fontSize:11,color:"rgba(255,255,255,0.6)",fontWeight:600}}>{SP[l]}</span>}</span>);
            })}
            {ui.collected.length===0&&<span style={{color:"#8a7b6b",fontSize:14,padding:"12px 0",textAlign:"center",fontWeight:400,lineHeight:1.5}}>Eat letters<br/>to collect</span>}
          </div>

          <div style={{borderTop:"1px solid #EDE3D4",marginTop:12,paddingTop:10,textAlign:"center"}}>
            <div style={{color:"#8a7b6b",fontSize:13,marginTop:6,fontWeight:400}}>Press <span style={{color:"#C67B5C",fontWeight:700,fontFamily:FM}}>W</span> for Word Mode</div>
            {ui.boosts>0&&<div style={{color:"#5D4037",fontSize:13,marginTop:4,fontWeight:600}}>Hold <span style={{fontFamily:FM,fontWeight:700}}>&#8593;</span> for Boost ({ui.boosts})</div>}
          </div>

          <div style={{borderTop:"1px solid #EDE3D4",marginTop:12,paddingTop:10}}>
            <div style={{display:"grid",gridTemplateColumns:"1fr 1fr",gap:6}}>
              <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"5px 4px",textAlign:"center"}}>
                <div style={{color:"#D4A574",fontSize:9,fontWeight:600,letterSpacing:"0.5px"}}>LETTERS</div>
                <div style={{color:"#C67B5C",fontSize:20,fontWeight:800,fontFamily:FN}}>{ui.totalLettersEaten}</div>
              </div>
              <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"5px 4px",textAlign:"center"}}>
                <div style={{color:"#D4A574",fontSize:9,fontWeight:600,letterSpacing:"0.5px"}}>TIME</div>
                <div style={{color:"#C67B5C",fontSize:16,fontWeight:800,fontFamily:FN}}>{Math.floor(ui.totalElapsed/60)}:{(ui.totalElapsed%60).toString().padStart(2,"0")}</div>
              </div>
            </div>
            <div style={{display:"grid",gridTemplateColumns:"1fr 1fr",gap:6,marginTop:6}}>
              {stats&&stats.longest&&(
                <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"5px 4px",textAlign:"center"}}>
                  <div style={{color:"#D4A574",fontSize:9,fontWeight:600,letterSpacing:"0.5px"}}>LONGEST</div>
                  <div style={{color:"#3E2723",fontSize:13,fontWeight:700,fontFamily:FM}}>{stats.longest.word}</div>
                </div>
              )}
              <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"5px 4px",textAlign:"center"}}>
                <div style={{color:"#D4A574",fontSize:9,fontWeight:600,letterSpacing:"0.5px"}}>DISTANCE</div>
                <div style={{color:"#C67B5C",fontSize:16,fontWeight:800,fontFamily:FN}}>{ui.totalDistance}</div>
              </div>
            </div>
          </div>

          {ui.hasAi&&(
            <div style={{borderTop:"1px solid #EDE3D4",marginTop:12,paddingTop:8,textAlign:"center"}}>
              <div style={{color:"#b33030",fontSize:12,fontWeight:700,letterSpacing:"0.5px"}}>ENEMY SNAKE ACTIVE</div>
              <div style={{color:"#8a7b6b",fontSize:11,fontWeight:400,marginTop:2}}>Hunting your vowels</div>
            </div>
          )}
          {ui.hasFalling&&(
            <div style={{borderTop:"1px solid #EDE3D4",marginTop:8,paddingTop:8,textAlign:"center"}}>
              <div style={{color:"#5D4037",fontSize:12,fontWeight:700,letterSpacing:"0.5px"}}>LETTERS FALLING</div>
              <div style={{color:"#8a7b6b",fontSize:11,fontWeight:400,marginTop:2}}>Catch them as they drop</div>
            </div>
          )}

          {stats&&(
            <div style={{borderTop:"1px solid #EDE3D4",marginTop:12,paddingTop:10}}>
              <div style={{color:"#D4A574",fontSize:11,textAlign:"center",marginBottom:8,fontWeight:700,letterSpacing:"1.5px",textTransform:"uppercase"}}>Stats</div>
              <div style={{display:"grid",gridTemplateColumns:"1fr 1fr",gap:6}}>
                <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#D4A574",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>TOTAL</div>
                  <div style={{color:"#C67B5C",fontSize:26,fontWeight:800,fontFamily:FN}}>{stats.total}</div>
                </div>
                <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#D4A574",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>UNIQUE</div>
                  <div style={{color:"#C67B5C",fontSize:26,fontWeight:800,fontFamily:FN}}>{ui.uniqueWordCount}</div>
                </div>
                <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#D4A574",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>BEST</div>
                  <div style={{color:"#C67B5C",fontSize:14,fontWeight:700,fontFamily:FM}}>{stats.best.word}</div>
                  <div style={{color:"#8a7b6b",fontSize:10,fontWeight:500}}>+{stats.best.score}</div>
                </div>
                <div style={{background:"#ede3d4",border:"1px solid #EDE3D4",borderRadius:8,padding:"6px 4px",textAlign:"center"}}>
                  <div style={{color:"#D4A574",fontSize:10,fontWeight:600,letterSpacing:"0.5px"}}>LONGEST</div>
                  <div style={{color:"#3E2723",fontSize:14,fontWeight:700,fontFamily:FM}}>{stats.longest.word}</div>
                  <div style={{color:"#8a7b6b",fontSize:10,fontWeight:500}}>{stats.longest.word.length} letters</div>
                </div>
              </div>
            </div>
          )}

          {ui.wordsFormed.length>0&&(
            <div style={{borderTop:"1px solid #EDE3D4",marginTop:12,paddingTop:10}}>
              <div style={{color:"#D4A574",fontSize:11,textAlign:"center",marginBottom:6,fontWeight:700,letterSpacing:"1.5px",textTransform:"uppercase"}}>Recent</div>
              <div style={{display:"flex",flexDirection:"column",gap:4}}>
                {ui.wordsFormed.slice(-5).reverse().map((w,i)=>(
                  <div key={i} style={{fontSize:12,padding:"4px 8px",borderRadius:6,textAlign:"center",fontWeight:600,fontFamily:FM,letterSpacing:"0.5px",
                    background:w.penalty?"#fdeaea":"#faf3e8",
                    border:`1px solid ${w.penalty?"#e8c0c0":"#d4c4b0"}`,
                    color:w.penalty?"#c0392b":"#C67B5C"
                  }}>{w.word}{w.combo>1?` x${w.combo}`:""} +{w.score}</div>
                ))}
              </div>
            </div>
          )}

        </div>
      </div>

    </div>
  );
}
