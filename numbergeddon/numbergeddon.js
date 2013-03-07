var Keys = { 
  Up: 38, Down: 40, Left: 37, Right: 39,
  W: 87, S: 83, A: 65, D: 68, BackSpace: 8,
  Return: 13
};

var Sprites = {
  Explosion: new Sprite({ 
    src: 'sprites/explosion.png',
    width: 64, height: 64, rows: 5, cols: 5
  })
};

var Numbergeddon = (function(){
  var keyHandlers = {};
  function Numbergeddon(spec){
    var self = this;
    self.width = spec.width;
    self.height = spec.height;
    self.canvas = document.getElementById(spec.id);
    self.players = [];
    self.keyHooks = [];
    var ctx = self.ctx = self.canvas.getContext('2d');
    self.keysPressed = {};
    var typer = self.typer = new Typer({
      grid: self,
      retTrigger: function(text){ self.killEnemies(text); }
    });
    var osd = self.osd = new OnscreenDisplay({
      grid: self
    });
    var player = self.player = new Player({ 
      grid: self, ctx: ctx, x: self.width/2, y: self.height/2 
    });
    player.render();
    self.pickupKeys();
    self.handleKeyEvents();
    var img = new Image();
    img.onload = function(){
      setInterval(function(){
        ctx.drawImage(img,0,0);
        typer.render();
        for (var i = 0; i < self.players.length; i++) {
          self.players[i].render();
        }
        self.player.render();
        if (!self.gracePeriod) {
          self.collisionDetect(self.player);
        }
        if (self.gameIsOver) {
          self.renderGameOver();
          self.renderNewGame();
        }
        for (var i = 0; i < self.players.length; i++) {
          var me = self.players[i];
          if (me.enemy) {
            me.follow();
          }
          me.velocity();
        }
        self.player.velocity();
        self.garbageCollect();
      },20);
      self.makeEnemies(player);
      self.setGracePeriod();
    }
    img.src = 'space.png';
  }
  keyHandlers[Keys.Up] = function(){
    this.player.accelUp();
  };
  keyHandlers[Keys.Down] = function(){
    this.player.accelDown();
  };
  keyHandlers[Keys.Left] = function(){
    this.player.accelLeft();
  };
  keyHandlers[Keys.Right] = function(){
    this.player.accelRight();
  };
  keyHandlers[Keys.W] = function(){
    this.player.accelUp();
  };
  keyHandlers[Keys.S] = function(){
    this.player.accelDown();
  };
  keyHandlers[Keys.A] = function(){
    this.player.accelLeft();
  };
  keyHandlers[Keys.D] = function(){
    this.player.accelRight();
  };
  keyHandlers[Keys.Return] = function(){
    if (this.gameIsOver) {
      this.newGame();
    }
  };
  Numbergeddon.prototype.newGame = function(){
    this.deleteEnemies();
    this.player.restart();
    this.setGracePeriod();
    this.makeEnemies(this.player);
    this.gameIsOver = false;
    this.typer.unlock();
  };
  Numbergeddon.prototype.deleteEnemies = function(){
    for (var i = 0; i < this.players.length; i++) {
      this.players[i].delete;
    }
    this.players = [];
  };
  Numbergeddon.prototype.setGracePeriod = function(){
    var self = this;
    self.gracePeriod = true;
    setInterval(function(){
      self.gracePeriod = false;
    },2000);
  };
  Numbergeddon.prototype.makeEnemies = function(player){
    for (var i = 0; i < 15; i++) {
      this.addAIPlayer({
        enemy: player,
        randomPosition: true
      });
    }
  }
  Numbergeddon.prototype.renderNewGame = function(){
    var ctx = this.ctx;
    var text = 'Press ENTER to play again.';
    var height = 15;
    ctx.save();
    ctx.shadowOffsetX = 3;
    ctx.shadowOffsetY = 3;
    ctx.shadowBlur = 2;
    ctx.shadowColor = "rgba(0, 0, 0, 1)";
    ctx.font = "bold "+height+"px Sans";
    ctx.fillStyle = "rgb(200,200,200)";
    ctx.textBaseline = "top";
    var dim = ctx.measureText(text);
    ctx.fillText(text,
                 (this.width/2)-(dim.width/2),
                 ((this.height/2)-(height/2)) + 100);
    ctx.restore();
  };
  Numbergeddon.prototype.collisionDetect = function(player){
    if (player.exploding) return;
    for (var i = 0; i < this.players.length; i++) {
      var enemy = this.players[i];
      if (enemy.exploding) continue;
      if (Math.distance(enemy,player) < 7) {
        player.explode();
        this.gameOver();
        break;
      }
    }
  };
  Numbergeddon.prototype.gameOver = function(){
    this.typer.lock();
    for (var i = 0; i < this.players.length; i++) {
      delete this.players[i].enemy;
    }
    this.gameIsOver = true;
  };
  Numbergeddon.prototype.renderGameOver = function(){
    var self = this;
    var ctx = this.ctx;
    var game_over = "GAME OVER";
    var height = self.gameOverHeight?self.gameOverHeight:10;
    if (height<60) {
      self.gameOverHeight = height + 2;
    }
    ctx.save();
    ctx.shadowOffsetX = 10;
    ctx.shadowOffsetY = 10;
    ctx.shadowBlur = 2;
    ctx.shadowColor = "rgba(0, 0, 0, 1)";
    ctx.font = "bold "+height+"px Sans";
    ctx.fillStyle = "rgb(200,200,200)";
    var text = this.currentText + this.cursor;
    ctx.textBaseline = "top";
    var dim = ctx.measureText(game_over);
    ctx.fillText(game_over,
                 (this.width/2)-(dim.width/2),
                 (this.height/2)-(height/2));
    ctx.restore();
  };
  Numbergeddon.prototype.garbageCollect = function(){
    var players = this.players;
    for (var i = 0; i < players.length; i++) {
      var player = players[i];
      if (player.dead) {
        players.splice(i,1);
        i--;
        delete player;
      }
    }
  };
  Numbergeddon.prototype.killEnemies = function(text){
    var killedAny = false;
    var players = this.players;
    for (var i = 0; i < players.length; i++) {
      if (players[i].question.value*1 == text*1) {
        killedAny = true;
        players[i].explode();
      }
    }
    if (!killedAny) {
    } 
  };
  Numbergeddon.prototype.shake = function(){
    var self = this;
    var shake = (0.2 * Math.random()) - 0.05;
    setTimeout(function(){
      self.ctx.rotate(shake);
      setTimeout(function(){
        self.ctx.rotate(-shake);
      },10);
    },10);
  };
  Numbergeddon.prototype.addAIPlayer = function(spec){
    var self = this;
    var x = 0, y = 0;
    if (spec.randomPosition) {
      x = Math.random()*self.width;
      y = Math.random()*self.height;
    }
    var player = new AIPlayer({ grid: self, ctx: self.ctx, x: x, y: y });
    player.render();
    player.startAttack(spec.enemy);
    this.players.push(player);
    return player;
  };
  Numbergeddon.prototype.pickupKeys = function(){
    var self = this;
    $(document).keydown(function(ev){
      self.keysPressed[ev.keyCode] = true;
      for (var i = 0; i < self.keyHooks.length; i++) {
        self.keyHooks[i](ev.keyCode);
      }
    });
    $(document).keyup(function(ev){
      delete self.keysPressed[ev.keyCode];
    });
  };
  Numbergeddon.prototype.handleKeyEvents = function(){
    var self = this;
    setInterval(function(){
      for (var key in self.keysPressed) {
        if (keyHandlers[key]) {
          keyHandlers[key].call(self);
        }
      }
    },10);
  };
  Numbergeddon.prototype.addKeyHook = function(hook){
    this.keyHooks.push(hook);
  };
  return Numbergeddon;
})();

var Typer = (function(){
  function click(){};
  function Typer(spec){
    var self = this;
    this.grid = spec.grid;
    this.x = 0;
    this.y = 0;
    this.currentText = '';
    this.cursor = '_';
    this.fillStyle = "rgb(200,200,200)";
    this.setCursorInterval();
    this.retTrigger = spec.retTrigger;
    this.grid.addKeyHook(function(keyCode){
      if (self.locked) {
        return;
      } else if (keyCode == Keys.BackSpace) {
        self.currentText = 
          self.currentText.substring(0,self.currentText.length-1);
        click();
      } else if (keyCode == Keys.Return) {
        self.lock();
        self.trigger();
        self.flash(function(){ self.clear(); self.unlock(); });
      } else if (self.currentText.length == 5) {
      } else if (keyCode >= 49 && keyCode <= 57) {
        self.currentText += keyCode - 48;
        click();
      } else if (keyCode >= 96 && keyCode <= 96+10) {
        self.currentText += keyCode - 96;
        click(); 
      } else if (keyCode == 48) {
        self.currentText += '0';
        click();
      } else if ((keyCode == 189 || keyCode == 109) && self.currentText == '') {
        self.currentText += '-';
        click();
      }
    });
  }
  Typer.prototype.trigger = function(){
    if (this.retTrigger) this.retTrigger(this.currentText);
  }
  Typer.prototype.clear = function(){
    this.currentText = '';
  };
  Typer.prototype.flash = function(cont){
    var self = this, intervals = 0;
    self.fillStyle = "rgb(0,150,250)";
    self.flashInterval = setInterval(function(){
      if (intervals > 3) {
        clearTimeout(self.flashInterval);
        self.fillStyle = "rgb(200,200,200)";
        if (cont) cont();
        return;
      }
      if (self.currentTextReal) {
        self.currentText = self.currentTextReal;
        delete self.currentTextReal;
      } else {
        self.currentTextReal = self.currentText;
        self.currentText = '';
      }
      intervals++;
    },40);
  };
  Typer.prototype.lock = function(){
    this.locked = true;
    this.cursor = '';
    clearTimeout(this.cursorInterval);
  };
  Typer.prototype.unlock = function(){
    this.locked = false;
    this.setCursorInterval();
  };
  Typer.prototype.setCursorInterval = function(){
    var self = this;
    this.cursorInterval = setInterval(function(){
      self.cursor = self.cursor=='_'? '' : '_';
    },350);
  };
  Typer.prototype.render = function(){
    var grid = this.grid;
    var ctx = grid.ctx;
    ctx.save();
    ctx.shadowOffsetX = 2;
    ctx.shadowOffsetY = 2;
    ctx.shadowBlur = 2;
    ctx.shadowColor = "rgba(0, 0, 0, 1)";
    ctx.font = "bold 20px Sans";
    ctx.fillStyle = this.fillStyle;
    var text = this.currentText + this.cursor;
    ctx.textBaseline = "top";
    var dim = ctx.measureText(this.currentText + '_');
    ctx.fillText(text,
                 (grid.width/2)-(dim.width/2),
                 grid.height-35);
    ctx.restore();
  };
  return Typer;
})();

var OnscreenDisplay = (function(){
  function OnscreenDisplay(spec){
  }
  OnscreenDisplay.prototype.render = function(){
  };
  return OnscreenDisplay;
})();

var Player = (function(){
  function Player(spec){
    if (spec) {
      this.speed = 0.2
      this.maxAccel = 4;
      this.ctx = spec.ctx;
      this.r = 0;
      this.x = spec.x, this.y = spec.y;
      this.w = 10, this.h = 10;
      this.xAccel = 0, this.yAccel = 0;
      this.fillStyle = "rgb(0,200,255)";
      this.grid = spec.grid;
    }
  }
  Player.prototype.restart = function(){
    this.explosion = false;
    this.exploding = false;
    this.dead = false;
  };
  Player.prototype.accelUp = function(){
    this.r = (this.r)  /2;
    var maxAccel = this.maxAccel;
    this.yAccel = Math.inRange(-maxAccel,maxAccel,this.yAccel-this.speed);
  };
  Player.prototype.accelDown = function(){
    this.r = (this.r + Math.PI)  /2;
    var maxAccel = this.maxAccel;
    this.yAccel = Math.inRange(-maxAccel,maxAccel,this.yAccel+this.speed);
  };
  Player.prototype.accelRight = function(){
    this.r = (this.r + (Math.PI/2))  /2;
    var maxAccel = this.maxAccel;
    this.xAccel = Math.inRange(-maxAccel,maxAccel,this.xAccel+this.speed);
  };
  Player.prototype.accelLeft = function(){
    this.r = (this.r + (Math.PI*1.5))  /2;
    var maxAccel = this.maxAccel;
    this.xAccel = Math.inRange(-maxAccel,maxAccel,this.xAccel-this.speed);
  };
  Player.prototype.render = function(){
    if (this.explosion) {
      if (this.explosion.ended)
        this.dead = true;
      else
        this.explosion.render();
    }
    else
      this.unitRender();
  };
  Player.prototype.unitRender = function(){
    var x = this.x, y = this.y, w = this.w, h = this.h;
    var ctx = this.ctx;
    ctx.save();
    ctx.beginPath();
    ctx.translate(x,y);
    ctx.rotate(this.r);
    ctx.translate(-(w/2),-(h/2));
    ctx.fillStyle = this.fillStyle;
    ctx.linesTo([
      [0,h],
      [w/2,h-(h*0.2)],
      [w,h],
      [w/2,0]
    ]);
    ctx.fill();
    ctx.restore();
  };
  var bumpInterval;
  Player.prototype.bump = function(){
    if (bumpInterval) return;
    bumpInterval = setInterval(function(){
      clearTimeout(bumpInterval);
      bumpInterval = undefined;
    },500);
  }
  Player.prototype.velocity = function(){
    var grid = this.grid;
    var x = this.x + this.xAccel;
    var y = this.y + this.yAccel;
    if (x > grid.width || x < 0) { this.bump(); this.xAccel *= -1 }
    if (y > grid.height || y < 0) { this.bump(); this.yAccel *= -1 }
    this.x = x;
    this.y = y;
    this.xAccel = this.xAccel * 0.99;
    this.yAccel = this.yAccel * 0.99;
  };
  Player.prototype.explode = function(){
    var self = this;
    self.exploding = true;
    self.explosion = new AnimateSprite({ 
      ctx: self.ctx,
      sprite: Sprites.Explosion,
      speed: 5,
      x: self.x-(Sprites.Explosion.width/2),
      y: self.y-(Sprites.Explosion.height/2)
    });
  };
  return Player;
})();

var AIPlayer = (function(){
  function AIPlayer(spec){
    Player.call(this,spec);
    this.maxAccel = 2;
    this.speed = 0.2;
    this.question = new Question({
      count: 3,
      operators: ['+','-']
    });
  };
  AIPlayer.prototype = new Player;
  AIPlayer.prototype.bump = function(){};
  AIPlayer.prototype.render = function(){
    Player.prototype.render.call(this);
    if (!this.explosion) {
      this.renderQuestion();
    }
  };
  AIPlayer.prototype.renderQuestion = function(){
    var ctx = this.ctx;
    ctx.save();
    ctx.font = "13px Sans";
    ctx.fillStyle = "rgb(200,200,200)";
    var text = this.question.expr;
    ctx.textBaseline = "top";
    ctx.fillText(text, this.x, this.y);
    ctx.restore();
  };
  AIPlayer.prototype.explode = function(){
    clearTimeout(this.attackTimer);
    Player.prototype.explode.call(this);
  };
  AIPlayer.prototype.startAttack = function(player){
    var self = this; var grid = self.grid;
    if (!self.attackTimer) {
      self.enemy = player;
      self.fillStyle = "rgb(255,200,200)";
      self.enemyCoords = { x: Math.random()*grid.width,
                           y: Math.random()*grid.height };
    };
  };
  AIPlayer.prototype.follow = function(){
    var self = this;
    var grid = self.grid;
    var enemy = self.enemyCoords;
    var realdistance = Math.distance(self,self.enemy);
    var distance = Math.distance(self,self.enemyCoords);
    if (realdistance < 100 || Math.random()>0.999) {
      self.maxAccel = 3;
      self.fillStyle = "rgb(255,50,50)";
      self.enemyCoords = { x: self.enemy.x, y: self.enemy.y };
    } else if (distance < 20) {
      self.maxAccel = 2;
      self.fillStyle = "rgb(255,200,200)";
      self.enemyCoords = { x: Math.random()*grid.width,
                           y: Math.random()*grid.height };
    }
    if (enemy.x < self.x) self.accelLeft();
    else if (enemy.x > self.x) self.accelRight();
    if (enemy.y > self.y) self.accelDown();
    else if (enemy.y < self.y) self.accelUp();
  };
  return AIPlayer;
})();

var Question = (function(){
  function Question(spec){
    var count = spec.count;
    var operators = spec.operators;
    var expr = [];
    if (!(count%2)) count--;
    for (var i = 0; i < count; i++) {
      var operator =
        operators[Math.round((100*Math.random())%(operators.length-1))];
      expr.push(i%2? operator : rnum());
    }
    this.expr = expr.join('');
    this.value = eval(expr.join('').replace(/x/g,'*'));
  };
  Question.prototype.toString = function(){
    
  };
  function rnum(){ return Math.round(Math.random()*30); }
  return Question;
})();

var AnimateSprite = (function(){
  function AnimateSprite(spec){
    var self = this;
    self.ctx = spec.ctx;
    self.sprite = spec.sprite;
    self.col = 0;
    self.row = 0;
    self.x = spec.x;
    self.y = spec.y;
    var sprite = self.sprite;
    var interval = setInterval(function(){
      if (self.col == sprite.cols) {
        self.col = 0; self.row++;
      } else if (self.row == sprite.rows) {
        self.col = 0; self.row = 0;
        // Reached end of animation
        if (!self.loop) {
          clearTimeout(interval);
          self.ended = true;
        }
      } else {
        self.col++;
      }
    },spec.speed);
  }
  AnimateSprite.prototype.render = function(){
    var ctx = this.ctx;
    var sprite = this.sprite;
    var w = sprite.width, h = sprite.height;
    ctx.save();
    ctx.translate(this.x,this.y);
    ctx.beginPath();
    ctx.linesTo([
      [0,0],
      [w,0],
      [w,h],
      [0,h]
    ])
    ctx.clip();
    ctx.drawImage(sprite.img,-(w*this.col),-(h*this.row));
    ctx.restore();
  };
  return AnimateSprite;
})();

function Sprite(spec){
  spec.img = new Image();
  spec.img.src = spec.src;
  return spec;
};

CanvasRenderingContext2D.prototype.linesTo = function(points){
  for (var i = 0; i < points.length; i++) {
    var x = points[i];
    this.lineTo(x[0],x[1]);
  }
};

Math.inRange = function(x,y,z) {
  return Math.max(x,Math.min(y,z));
};

Math.avg = function(x,y){ 
  return x+y/2; 
};

Math.distance = function(a,b){
  return Math.sqrt(Math.pow(Math.abs(a.x-b.x),2)+
                   Math.pow(Math.abs(a.y-b.y),2));
};
