%{
	//float pointers
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"

#include "list.h"
#include "symbol.h"

int num_errors;
char * id;
int count;
extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror();
int parser_error(const char*);

void minic_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;
LLVMValueRef case_expr;
 LLVMBuilderRef Builder;
LLVMBasicBlockRef join_g,BB1_g,BB2_g,case_join;
LLVMValueRef Function=NULL;
LLVMValueRef switch_val;
LLVMValueRef ans_BB1;
LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params);

%}

/* Data structure for tree nodes*/

%union {
  int inum;
  float fnum;
  char * id;
  LLVMTypeRef  type;
  LLVMValueRef value;
  LLVMBasicBlockRef bb;
  paramlist_t *params;
}

/* these tokens are simply their corresponding int values, more terminals*/

%token SEMICOLON COMMA COLON
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ NOT
%token LOGICAL_AND LOGICAL_OR
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT

%token DOT ARROW AMPERSAND QUESTION_MARK

%token FOR WHILE IF ELSE DO STRUCT SIZEOF RETURN SWITCH
%token BREAK CONTINUE CASE DEFAULT
%token INT VOID FLOAT

/* no meaning, just placeholders */
%token STATIC AUTO EXTERN TYPEDEF CONST VOLATILE ENUM UNION REGISTER
/* NUMBER and ID have values associated with them returned from lex*/

%token <inum> CONSTANT_INTEGER /*data type of NUMBER is num union*/
%token <fnum> CONSTANT_FLOAT /*data type of NUMBER is num union*/
%token <id>  ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* values created by parser*/

%type <id> declarator
%type <params> param_list param_list_opt
%type <value> expression
%type <value> assignment_expression
%type <value> conditional_expression
%type <value> constant_expression
%type <value> logical_OR_expression
%type <value> logical_AND_expression
%type <value> inclusive_OR_expression
%type <value> exclusive_OR_expression
%type <value> AND_expression
%type <value> equality_expression
%type <value> relational_expression
%type <value> shift_expression
%type <value> additive_expression
%type <value> multiplicative_expression
%type <value> cast_expression
%type <value> unary_expression
%type <value> lhs_expression
%type <value> postfix_expression
%type <value> primary_expression
%type <value> constant
%type <value> expr_opt
%type <type> type_specifier
%type <value> opt_initializer
/* 
   The grammar used here is largely borrowed from Kernighan and Ritchie's "The C
   Programming Language," 2nd Edition, Prentice Hall, 1988. 

   But, some modifications have been made specifically for MiniC!
 */

%%

/* 
   Beginning of grammar: Rules
*/

translation_unit:	  external_declaration
			| translation_unit external_declaration
;

external_declaration:	  function_definition
{
  /* finish compiling function */
  if(num_errors>100)
    {
      minic_abort();
    }
  else if(num_errors==0)
    {
      
    }
}
                        | declaration 
{ 
  /* nothing to be done here */
}
;

function_definition:	  type_specifier ID LPAREN param_list_opt RPAREN 
{
	
  symbol_push_scope();
  /* This is a mid-rule action */
  BuildFunction($1,$2,$4);  
} 
                          compound_stmt 
{ 
  /* This is the rule completion */
  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      if($1==LLVMInt32Type())	
	{
		
	  LLVMBuildRet(Builder,LLVMConstInt(LLVMInt32TypeInContext(Context),
					    0,(LLVMBool)1));
		  
	}
      else if($1==LLVMFloatType()) 
	{
	  LLVMBuildRet(Builder,LLVMConstReal(LLVMFloatType(),0.0));
				
	}
      else
	{
	  LLVMBuildRetVoid(Builder);
	
	}
    }

  symbol_pop_scope();
  /* make sure basic block has a terminator (a return statement) */
}
                        | type_specifier STAR ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  if($1==LLVMVoidType())
  {
	 BuildFunction(LLVMPointerType(LLVMInt8Type(),0),$3,$5); 
  }
  else
  BuildFunction(LLVMPointerType($1,0),$3,$5);
} 
                          compound_stmt 
{ 
  /* This is the rule completion */


  /* make sure basic block has a terminator (a return statement) */

  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
		if($1==LLVMVoidType())
		{
			LLVMBuildRet(Builder,LLVMConstPointerNull(LLVMPointerType(LLVMInt8Type(),0)));
		}
		else
      LLVMBuildRet(Builder,LLVMConstPointerNull(LLVMPointerType($1,0)));
    }

  symbol_pop_scope();
}
;

declaration:    type_specifier STAR ID opt_initializer SEMICOLON
{
	
  if (is_global_scope())
    {
      LLVMValueRef g =  LLVMAddGlobal(Module,LLVMPointerType($1,0),$3);
		if($4!=NULL)
		{
			 LLVMSetInitializer(g,$4);
		}
     
    } 
  else
    {
		if($1==LLVMVoidType())
		{
			symbol_insert($3,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,LLVMPointerType(LLVMInt8Type(),0),$3), /* build alloca */
		    0); 
		}
		else
		{
      symbol_insert($3,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,LLVMPointerType($1,0),$3), /* build alloca */
		    0);  /* not an arg */
		}
			LLVMValueRef val = symbol_find($3,NULL);
       if($4!=NULL)
	  {

       LLVMBuildStore(Builder,$4, val);
	  }
      
    }

} 
              | type_specifier ID opt_initializer SEMICOLON
{
	
  if (is_global_scope())
    {
      LLVMValueRef g =  LLVMAddGlobal(Module,$1,$2);
		if($3!=NULL)
			{
		
				 LLVMSetInitializer(g,$3);
			}
      
    }
  else
    {
      symbol_insert($2,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,$1,$2), /* build alloca */
		    0);  /* not an arg */

      // Store initial value if there is one
	  LLVMValueRef val = symbol_find($2,NULL);
	  if($3!=NULL)
	  {
	
       LLVMBuildStore(Builder,$3, val);
	  }
    }
} 
;

declaration_list:	   declaration
{

}
                         | declaration_list declaration  
{

}
;


type_specifier:		  INT 
{
  $$ = LLVMInt32Type();
}
|                         FLOAT
{
  $$ = LLVMFloatType();
}
|                         VOID
{
  $$ = LLVMVoidType();
}
;

declarator: ID
{
  $$ = $1;
}
;

opt_initializer: ASSIGN constant_expression	      
{
  $$ = $2;
}
| // nothing
{
  // indicate there is none
  $$ = NULL;
}
;

param_list_opt:           
{ 
  $$ = NULL;
}
                        | param_list
{ 
  $$ = $1;
}
;

param_list:	
			  param_list COMMA type_specifier declarator
{
  $$ = push_param($1,$4,$3);
}
			| param_list COMMA type_specifier STAR declarator
{
  $$ = push_param($1,$5,LLVMPointerType($3,0));
}
                        | param_list COMMA type_specifier
{
  $$ = push_param($1,NULL,$3);
}
			|  type_specifier declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $2, $1);
}
			| type_specifier STAR declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $3, LLVMPointerType($1,0));
}
                        | type_specifier
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, NULL, $1);
}
;


statement:		  expr_stmt            
			| compound_stmt        
			| selection_stmt       
			| iteration_stmt       
			| jump_stmt            
                        | break_stmt
                        | continue_stmt
                        | case_stmt
						| default_stmt
;
default_stmt:				DEFAULT COLON
{
	 
	loop_info_t loop= get_loop();
	LLVMBuildBr(Builder,loop.expr);
  LLVMPositionBuilderAtEnd(Builder,loop.expr);
	
};
expr_stmt:	           SEMICOLON            
{ 

}
			|  expression SEMICOLON       
{ 

}
;

compound_stmt:		  LBRACE declaration_list_opt statement_list_opt RBRACE 
{

}
;

declaration_list_opt:	
{

}
			| declaration_list
{

}
;

statement_list_opt:	
{

}
			| statement_list
{

}
;

statement_list:		statement
{

}
			| statement_list statement
{

}
;

break_stmt:               BREAK SEMICOLON
{
	/* get the loop basic blocks */
 loop_info_t loop= get_loop();
 /* make a break block */
 LLVMBasicBlockRef break_block=LLVMAppendBasicBlock(Function,"break.block");
 /* branch to loop's exit block */
  LLVMBuildBr(Builder,loop.exit); 
 LLVMPositionBuilderAtEnd(Builder,break_block);
 
};

case_stmt:                CASE constant_expression COLON
{
 
  LLVMBasicBlockRef case_prog=LLVMAppendBasicBlock(Function,"case_prog");
  LLVMAddCase	(switch_val,$2, case_prog);
 LLVMBuildBr(Builder,case_prog);
  LLVMPositionBuilderAtEnd(Builder,case_prog);
	
  
  
  
};

continue_stmt:            CONTINUE SEMICOLON
{
	/* get the loop basic blocks */
 loop_info_t loop= get_loop();
 /* make a continue block */
 LLVMBasicBlockRef continue_block=LLVMAppendBasicBlock(Function,"continue.block");
 /* branch to expression block of the loops */
 LLVMBuildBr(Builder,loop.expr);
 LLVMPositionBuilderAtEnd(Builder,continue_block);
  
};

selection_stmt:		  
		          IF LPAREN expression RPAREN 
				  {	LLVMValueRef three;
					  if(LLVMTypeOf($3)!=LLVMInt32Type() && LLVMTypeOf($3)!=LLVMFloatType() && LLVMTypeOf($3)!=LLVMVoidType())
					  {
						  three= LLVMBuildPtrToInt(Builder,$3,LLVMInt32Type(),""); 	
					  }
					  else
						  /* save the expression in three */
						  three=$3;
						  /* make then and else basic block */
					  LLVMBasicBlockRef then =LLVMAppendBasicBlock(Function,"ifthen.block");
					  LLVMBasicBlockRef elseb =LLVMAppendBasicBlock(Function,"ifelseb.block");
					  LLVMValueRef zero=LLVMConstInt(LLVMTypeOf(three),0,1);
					  /* compare conditon with zero */
					  LLVMValueRef cond= LLVMBuildICmp(Builder,LLVMIntNE,three,zero,"cond");
					  /* depending on the comparison  jump to then or else */
					  LLVMValueRef br= LLVMBuildCondBr(Builder,cond,then,elseb);
					  /* build then basic block */
					  LLVMPositionBuilderAtEnd(Builder,then);
					  $<bb>$=elseb;
					  
				  }
				  statement 
				  {
					  /* basic block for code after if-else*/
					  LLVMBasicBlockRef join =LLVMAppendBasicBlock(Function,"ifjoin.block");
					  /* branch to join block */
					  LLVMBuildBr(Builder,join);
					  $<bb>$=join;
					  /* build else block*/
					  LLVMPositionBuilderAtEnd(Builder,$<bb>5);
				  }
				  
				  ELSE statement 
				  {
					  /* branch to join block */
					  LLVMBuildBr(Builder,$<bb>7);
					  /* build join block */
					  LLVMPositionBuilderAtEnd(Builder,$<bb>7);
				  }
{ 

}
|  SWITCH 
{
	LLVMBasicBlockRef case_block=LLVMAppendBasicBlock(Function,"switch.case_block");
	 LLVMBuildBr(Builder,case_block);
	 LLVMPositionBuilderAtEnd(Builder,case_block);
	 
}

LPAREN expression RPAREN 
{
 LLVMBasicBlockRef defaultkkk_block=LLVMAppendBasicBlock(Function,"switch.defaultlll_block");
  LLVMBasicBlockRef default_block=LLVMAppendBasicBlock(Function,"switch.default_block");
  LLVMBasicBlockRef join_block=LLVMAppendBasicBlock(Function,"switch.join_block");
		case_join=join_block;
		  push_loop(default_block,join_block,join_block,join_block);
   LLVMValueRef switch_case= LLVMBuildSwitch(Builder,$4,default_block,1000);
   


	switch_val=switch_case;
	

  
}
statement 
{
	LLVMBuildBr(Builder,case_join);
   //loop_info_t info = get_loop();
	LLVMPositionBuilderAtEnd(Builder,case_join);
	pop_loop();
}
;

iteration_stmt:		  WHILE LPAREN { 
  /* set up header basic block
     make it the new insertion point */
	 /*make a basic block for condition */
	 LLVMBasicBlockRef cond=LLVMAppendBasicBlock(Function,"while.cond");
	 /* branch to condition */
	 LLVMBuildBr(Builder,cond);
	 $<bb>$=cond;
	 /* build cond branch */
	 LLVMPositionBuilderAtEnd(Builder,cond);
	 

} expression RPAREN { 
  /* set up loop body */
	LLVMBasicBlockRef body =LLVMAppendBasicBlock(Function,"while.body");
	LLVMBasicBlockRef join =LLVMAppendBasicBlock(Function,"while.join");
  /* create new body and exit blocks */
  LLVMValueRef zero=LLVMConstInt(LLVMTypeOf($4),0,1);
  LLVMValueRef cond=LLVMBuildICmp(Builder,LLVMIntNE,$4,zero,"");
  LLVMValueRef br = LLVMBuildCondBr(Builder,cond,body,join);
  /* build body basic block */
  LLVMPositionBuilderAtEnd(Builder,body);
  $<bb>$=join;
  /* to support nesting: */
  push_loop($<bb>3,body,body,join);
} 
  statement
{
  /* finish loop */
  loop_info_t info = get_loop();
  LLVMBuildBr(Builder,$<bb>3);
  LLVMPositionBuilderAtEnd(Builder,$<bb>6);
  pop_loop();
}

| FOR LPAREN expr_opt 
{
	/* make a basic block for forloop condition */
	 LLVMBasicBlockRef cond=LLVMAppendBasicBlock(Function,"for.cond");
	 /* Branch to basic block cond */
	 LLVMBuildBr(Builder,cond);
	 $<bb>$=cond;
	 /* Build basic block cond */
	 LLVMPositionBuilderAtEnd(Builder,cond);
 } 
SEMICOLON expr_opt 
{
	/* this expr_opt is the loop condition*/
	LLVMValueRef zero,cond;
	/* make three basic blocks */
	/* body of the loop */
	LLVMBasicBlockRef body =LLVMAppendBasicBlock(Function,"for.body");
	/* basic block after the loop ends*/
	LLVMBasicBlockRef join =LLVMAppendBasicBlock(Function,"for.join");
	/* basic block for incrementing the value to reenter the loop */
	LLVMBasicBlockRef reinit =LLVMAppendBasicBlock(Function,"for.reinit");
  /* create new body and exit blocks */
  /* create LLVMValueRef with value zero and compare if the expr_opt is not equal to zero */
  if(LLVMTypeOf($6)==LLVMInt32Type())
  {
	  zero=LLVMConstInt(LLVMTypeOf($6),0,1);
     cond=LLVMBuildICmp(Builder,LLVMIntNE,$6,zero,"");  
  }
  else
  {
  zero=LLVMConstReal(LLVMTypeOf($6),0);
  cond=LLVMBuildFCmp(Builder,LLVMRealONE,$6,zero,"");
  }
  /* Depending on the comparator jump to appropriate basic block */
  LLVMValueRef br = LLVMBuildCondBr(Builder,cond,body,join);
  /* build reinit block */
  LLVMPositionBuilderAtEnd(Builder,reinit);
  $<bb>$=join;
  /* to support nesting: */
  push_loop($<bb>4,body,reinit,join);
} 
SEMICOLON expr_opt 
{
	/* jump to condition block */
	LLVMBuildBr(Builder,$<bb>4);
	/* build body basic block */
	LLVMPositionBuilderAtEnd(Builder,get_loop().body);
}
RPAREN statement
{

  loop_info_t info = get_loop();
/* jump to condition reinit */
  LLVMBuildBr(Builder,get_loop().reinit);
  /* build join basic block */
  LLVMPositionBuilderAtEnd(Builder,$<bb>7);
  pop_loop();
  
}
;

expr_opt:		
{ 

}
			| expression
{ 
$$=$1;
}
;

jump_stmt:		  RETURN SEMICOLON
{ 
  LLVMBuildRetVoid(Builder);

}
			| RETURN expression SEMICOLON
{
  LLVMBuildRet(Builder,$2);
}
;

expression:               assignment_expression
{ 
  $$=$1;
}
;

assignment_expression:    conditional_expression
{
  $$=$1;
}
                        | lhs_expression ASSIGN assignment_expression
{
  	/* assign a value to a variable is literally storing a value */
   LLVMTypeRef int_ptr=LLVMPointerType(LLVMInt8Type(),0);
   LLVMTypeRef ptr_int_ptr=LLVMPointerType(int_ptr,0);
    LLVMTypeRef int32_ptr=LLVMPointerType(LLVMInt32Type(),0);
	LLVMTypeRef ptr_int32_ptr=LLVMPointerType(int32_ptr,0);
	/* if LHS is int */
  if(LLVMGetElementType(LLVMTypeOf($1))==LLVMInt32Type())
  {
	/* if RHS is float*/
	  if(LLVMTypeOf($3)==LLVMFloatType())
	  {
		  /* convert float to int */
		  LLVMValueRef val=LLVMBuildFPToSI(Builder,$3,LLVMInt32Type(),"");
		  
		  LLVMBuildStore(Builder,val,$1);
	  }
	  else
	  {
		LLVMBuildStore(Builder,$3,$1);  
	  }
  }
  /* if LHS is float */
  else if(LLVMGetElementType(LLVMTypeOf($1))== LLVMFloatType())
  {
	  if(LLVMTypeOf($3)==LLVMInt32Type())
	  {
		  LLVMValueRef val=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
		  LLVMBuildStore(Builder,val,$1);
	  }
	  else
	  {
		  LLVMBuildStore(Builder,$3,$1);
	  }
  }
  /* if LHS is a pointer */
  else if(LLVMTypeOf($1)== ptr_int_ptr)
  {
	  
	LLVMValueRef val=LLVMBuildBitCast(Builder,$3,int_ptr,"");	
	LLVMBuildStore(Builder,val,$1);
  }
  /* if RHS is a constant */
 else	 
  {
	  if($3==LLVMConstInt(LLVMInt32Type(),0,0))
	  {

		
		
		  LLVMValueRef val=LLVMConstPointerNull(LLVMPointerType(LLVMInt32Type(),0));
	
		  LLVMBuildStore(Builder,val,$1);
	  }
	  else
	  {
		  LLVMBuildStore(Builder,$3,$1);
	  }
	 
	  
  }
  
}
;


conditional_expression:   logical_OR_expression
{
  $$=$1;
}
                        | logical_OR_expression QUESTION_MARK expression COLON conditional_expression
{
  /* Make a LLVMValueRef of value zero */
  LLVMValueRef zero=LLVMConstInt(LLVMTypeOf($1),0,1);
  /* Build comparator to check is input is equal zero */
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntEQ,$1,zero,"logical.neg");
  /* jump to appropriate basic block with respect to comparator answer */
   $$=LLVMBuildSelect(Builder,icmp,$5,$3,"");
}
;

constant_expression:       conditional_expression
{ $$ = $1; }
;

logical_OR_expression:    logical_AND_expression
{
  $$ = $1;
}
                        | logical_OR_expression LOGICAL_OR 
						{
							/* store first input in one */
							 LLVMValueRef one;
							if(LLVMTypeOf($1)!=LLVMFloatType() && LLVMTypeOf($1)!=LLVMInt32Type() &&LLVMTypeOf($1)!=LLVMVoidType() )
							{
								one=LLVMBuildPtrToInt(Builder,$1,LLVMInt32Type(),"");
							}
							else
								one=$1;
							/* make three basic blocks */
								LLVMBasicBlockRef BB1=LLVMAppendBasicBlock(Function,"OR.BB1");
							LLVMBasicBlockRef BB2=LLVMAppendBasicBlock(Function,"OR.BB2");
							BB1_g=BB1;
							BB2_g=BB2;
							LLVMBasicBlockRef join =LLVMAppendBasicBlock(Function,"OR.join");
							/* make a LLVMValueRef of value zero */
							LLVMValueRef zero=LLVMConstInt(LLVMInt32Type(),0,0);
							/* if first input is not equal zero, jump to BB1 else jump to BB2 */
							LLVMValueRef cond=LLVMBuildICmp(Builder,LLVMIntNE,one,zero,"logical.OR");
							LLVMValueRef br = LLVMBuildCondBr(Builder,cond,BB1,BB2);
							$<bb>$=join;
							/* build basic block BB1 */
							 LLVMPositionBuilderAtEnd(Builder,BB1);
							
							count=0;
							ans_BB1=LLVMBuildZExt(Builder,cond,LLVMTypeOf(one),"logical.OR");
							/* jump to join */
							 LLVMBuildBr(Builder,join);
							 /* build basic block BB1 */
							 LLVMPositionBuilderAtEnd(Builder,BB2);
						}
						
						
						
						logical_AND_expression
{
  /* store second input in four */
  LLVMValueRef four;
   LLVMValueRef zero=LLVMConstInt(LLVMInt32Type(),0,0);
	if(LLVMTypeOf($4)!=LLVMFloatType() && LLVMTypeOf($4)!=LLVMInt32Type())
	{
		four=LLVMBuildPtrToInt(Builder,$4,LLVMInt32Type(),"");
	}
	else
		four=$4;
	/* if first input is not equal zero, jump to BB1 else jump to BB2 */
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntNE,four,zero,"logical.or");
	  LLVMValueRef ans=LLVMBuildZExt(Builder,icmp,LLVMTypeOf(four),"logical.or");
	/* jump to join */
	   LLVMBuildBr(Builder,$<bb>3);
	   /* build basic block join */
	   LLVMPositionBuilderAtEnd(Builder,$<bb>3);
	    /* if the arrival to join is through BB1, give the comparison output of BB1 */
	   /* if the arrival to join is through BB2, give the comparison output of BB2 */
	   /* phinode */
	   LLVMValueRef phinode = LLVMBuildPhi(Builder,LLVMTypeOf(four),"");
	   LLVMAddIncoming(phinode,&ans_BB1,&BB1_g,1);
	   LLVMAddIncoming(phinode,&ans,&BB2_g,1);
	   $$=phinode;
	 
  
  
};

logical_AND_expression:   inclusive_OR_expression
{
  $$ = $1;
}
                        | logical_AND_expression LOGICAL_AND 
						{
							LLVMValueRef one;
							if(LLVMTypeOf($1)!=LLVMFloatType() && LLVMTypeOf($1)!=LLVMInt32Type())
							{
								one=LLVMBuildPtrToInt(Builder,$1,LLVMInt32Type(),"");
							}
							else
								/* store first input in one */
								one=$1;
								/* make a LLVMValueRef of value zero */
							LLVMValueRef zero=LLVMConstInt(LLVMInt32Type(),0,0);
							/* make three basic block*/
							LLVMBasicBlockRef BB1=LLVMAppendBasicBlock(Function,"AND.BB1");
							LLVMBasicBlockRef BB2=LLVMAppendBasicBlock(Function,"AND.BB2");
							BB1_g=BB1;
							BB2_g=BB2;
							LLVMBasicBlockRef join =LLVMAppendBasicBlock(Function,"AND.join");
							/* if first input is zero, jump to BB1 else jump to BB2 */
							LLVMValueRef cond=LLVMBuildICmp(Builder,LLVMIntEQ,one,zero,"logical.and");
							LLVMValueRef br = LLVMBuildCondBr(Builder,cond,BB1,BB2);
							$<bb>$=join;
							/* build basic block BB1 */
							 LLVMPositionBuilderAtEnd(Builder,BB1);
							
							count=0;
							ans_BB1=zero;
							/* jump to join */
							 LLVMBuildBr(Builder,join);
							 /* build basic block BB2 */
							 LLVMPositionBuilderAtEnd(Builder,BB2);
							 
							
						}
						
						inclusive_OR_expression
{
  /* store the second input */
  LLVMValueRef four;
  /* make a LLVMValue ref of value zero for comparison */
   LLVMValueRef zero=LLVMConstInt(LLVMInt32Type(),0,0);
	if(LLVMTypeOf($4)!=LLVMFloatType() && LLVMTypeOf($4)!=LLVMInt32Type())
	{
		four=LLVMBuildPtrToInt(Builder,$4,LLVMInt32Type(),"");
	}
	else
		four=$4;
	/* compare if the second input is not equal to zero */
    LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntNE,four,zero,"logical.and");
	  LLVMValueRef ans=LLVMBuildZExt(Builder,icmp,LLVMTypeOf(four),"logical.and");
	
	  LLVMBuildBr(Builder,$<bb>3);
	  /* build basic block join */
	   LLVMPositionBuilderAtEnd(Builder,$<bb>3);
	   /* if the arrival to join is through BB1, give the comparison output of BB1 */
	   /* if the arrival to join is through BB2, give the comparison output of BB2 */
	   /* phinode */
	   LLVMValueRef phinode = LLVMBuildPhi(Builder,LLVMTypeOf(four),"");
	   LLVMAddIncoming(phinode,&ans_BB1,&BB1_g,1);
	   LLVMAddIncoming(phinode,&ans,&BB2_g,1);
	   $$=phinode;

	   

  
}

inclusive_OR_expression:  exclusive_OR_expression
{
    $$=$1;
}
                        | inclusive_OR_expression BITWISE_OR exclusive_OR_expression
{
  /* Implement */
  $$=LLVMBuildOr(Builder,$1,$3,"");
}
;

exclusive_OR_expression:  AND_expression
{
  $$ = $1;
}
                        | exclusive_OR_expression BITWISE_XOR AND_expression
{
  /* Implement */
  $$=LLVMBuildXor(Builder,$1,$3,"");
}
;

AND_expression:           equality_expression
{
  $$ = $1;
}
                        | AND_expression AMPERSAND equality_expression
{
  /* Implement */
  
  $$=LLVMBuildAnd(Builder,$1,$3,"");
 
}
;

equality_expression:      relational_expression
{
  $$ = $1;
}
                        | equality_expression EQ relational_expression
{
  /* Implemented comparator for both ints */
   if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntEQ,$1,$3,"relational.EQ");
   $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.EQ");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()))
  {
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOEQ,$1,arg2,"relational.EQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.EQ");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOEQ,arg1,$3,"relational.EQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.EQ");
  }
  /* Implemented comparator for both floats */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOEQ,arg1,arg2,"relational.EQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.EQ");
  }
  else 
	   /* Implemented comparator if inputs are pointers */
  {
	    LLVMValueRef arg1 =LLVMBuildPtrToInt(Builder,$1,LLVMInt32Type(),"");
	   LLVMValueRef arg2 =LLVMBuildPtrToInt(Builder,$3,LLVMInt32Type(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMIntEQ,arg1,arg2,"relational.EQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.EQ");
  }

}
                        | equality_expression NEQ relational_expression
{
	/* Implemented comparator for both ints */
   if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntNE,$1,$3,"relational.NEQ");
   $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.NEQ");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()))
  {
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealONE,$1,arg2,"relational.NEQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.NEQ");
  }
  else if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealONE,arg1,$3,"relational.NEQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.NEQ");
  }
  /* Implemented comparator for both floats */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealONE,arg1,arg2,"relational.NEQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.NEQ");
  }
  /* Implemented comparator if inputs are pointers */
  else 
  {
	    LLVMValueRef arg1 =LLVMBuildPtrToInt(Builder,$1,LLVMInt32Type(),"");
	   LLVMValueRef arg2 =LLVMBuildPtrToInt(Builder,$3,LLVMInt32Type(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMIntNE,arg1,arg2,"relational.NEQ");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.NEQ");
  }
}
;

relational_expression:    shift_expression
{
    $$=$1;
}
                        | relational_expression LT shift_expression
{
  /* Implemented comparator for both ints */
  if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntSLT,$1,$3,"relational.LT");
   $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.LT");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()))
  {
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOLT,$1,arg2,"relational.LT");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.LT");
  }
  else if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOLT,arg1,$3,"relational.LT");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.LT");
  }
   /* Implemented comparator for both floats */
  else
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOLT,arg1,arg2,"relational.LT");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.LT");
  }
	
}
                        | relational_expression GT shift_expression
{
  
  /* Implemented comparator for both ints */
   if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntSGT,$1,$3,"relational.GT");
   $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.GT");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()))
  {
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOGT,$1,arg2,"relational.GT");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.GT");
  }
  else if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOGT,arg1,$3,"relational.GT");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.GT");
  }
   /* Implemented comparator for both floats */
  else
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOGT,arg1,arg2,"relational.GT");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.GT");
  }
  
}
                        | relational_expression LTE shift_expression
{
  
  /* Implemented comparator for both ints */ 
  if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntSLE,$1,$3,"relational.LTE");
   $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.LTE");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()))
  {
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOLE,$1,arg2,"relational.LTE");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.LTE");
  }
  else if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOLE,arg1,$3,"relational.LTE");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.LTE");
  }
  /* Implemented comparator for both floats */
  else
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOLE,arg1,arg2,"relational.LTE");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.LTE");
  }
  
  
}
                        | relational_expression GTE shift_expression
{
  /* Implemented comparator for both ints */
  if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntSGE,$1,$3,"relational.GTE");
   $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"relational.GTE");
  }
  /* implement comparator for one int and other float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()))
  {
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOGE,$1,arg2,"relational.GTE");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.GTE");
  }
  else if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMFloatType()))
  {
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOGE,arg1,$3,"relational.GTE");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.GTE");
  }
   /* Implemented comparator for both floats */
  else
  {
	 
	  LLVMValueRef arg1 =LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2 =LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	    LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOGE,arg1,arg2,"relational.GTE");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"relational.GTE");
  }
}
;

shift_expression:         additive_expression
{
    $$=$1;
}
                        | shift_expression LSHIFT additive_expression
{
  /* Implemented left shift */
   	$$ =LLVMBuildShl(Builder,$1,$3,"");
}
                        | shift_expression RSHIFT additive_expression
{
  /* Implemented right shift */
  $$ =LLVMBuildLShr(Builder,$1,$3,"");
}
;
 
additive_expression:      multiplicative_expression
{
  $$ = $1;
}
                        | additive_expression PLUS multiplicative_expression
{
  /* Implemented addition if inputs are int */ 
   if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  $$=LLVMBuildAdd(Builder,$1,$3,"");
  }
  /* Implemented addition if one input are int anf other is float */ 
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFAdd(Builder,$1,arg2,"");
  }
  else if ((LLVMTypeOf($3)==LLVMFloatType())&&(LLVMTypeOf($1)==LLVMInt32Type()) )
  {
	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  $$=LLVMBuildFAdd(Builder,arg1,$3,"");
  }
  else 
	  /* Implemented addition if inputs are float */
  {
	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFAdd(Builder,arg1,arg2,"");
  }
}
                        | additive_expression MINUS multiplicative_expression
{
  /* Implemented subraction if inputs are int */
  if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  $$=LLVMBuildSub(Builder,$1,$3,"");
  }
  /* Implemented subtraction operation if one input is int and other is float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
	  //LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFSub(Builder,$1,arg2,"");
  }
  else if ((LLVMTypeOf($3)==LLVMFloatType())&&(LLVMTypeOf($1)==LLVMInt32Type()) )
  {
	  	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  $$=LLVMBuildFSub(Builder,arg1,$3,"");
  }
  else 
	  /* Implemented subraction if inputs are float */
  {
	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFSub(Builder,arg1,arg2,"");
  }
}
;

multiplicative_expression:  cast_expression
{
  $$ = $1;
}
                        | multiplicative_expression STAR cast_expression
{
  /* Implemented multiplication if inputs are int */  
  if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  $$=LLVMBuildMul(Builder,$1,$3,"");
  }
  /* Implemented multiplication operation if one input is int and other is float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFMul(Builder,$1,arg2,"");
  }
  else if ((LLVMTypeOf($3)==LLVMFloatType())&&(LLVMTypeOf($1)==LLVMInt32Type()) )
  {
	  	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  $$=LLVMBuildFMul(Builder,arg1,$3,"");
  }
  else 
	  /* Implemented multiplication if inputs are float */
  {
	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFMul(Builder,arg1,arg2,"");
  }
		
}
                        | multiplicative_expression DIV cast_expression
{
  /* Implemented division if inputs are int */
  if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
		$$=LLVMBuildSDiv(Builder,$1,$3,"");
  }
  /* Implemented division operation if one input is int and other is float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFDiv(Builder,$1,arg2,"");
  }
  else if ((LLVMTypeOf($3)==LLVMFloatType())&&(LLVMTypeOf($1)==LLVMInt32Type()) )
  {
	  	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  $$=LLVMBuildFDiv(Builder,arg1,$3,"");
  }
  else 
	  /* Implemented division if inputs are floats */
  {
	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFDiv(Builder,arg1,arg2,"");
  }
}
                        | multiplicative_expression MOD cast_expression
{
  /* Implemented mod operation if inputs are int */
   if((LLVMTypeOf($1)==LLVMInt32Type())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
  $$=LLVMBuildSRem(Builder,$1,$3,"");
  }
  /* Implemented mod operation if one input is int and other is float */
  else if((LLVMTypeOf($1)==LLVMFloatType())&&(LLVMTypeOf($3)==LLVMInt32Type()) )
  {
	  //LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFRem(Builder,$1,arg2,"");
  }
  else if ((LLVMTypeOf($3)==LLVMFloatType())&&(LLVMTypeOf($1)==LLVMInt32Type()) )
  {
	  	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  $$=LLVMBuildFRem(Builder,arg1,$3,"");
  }
  else 
	  /* Implemented mod operation if inputs are floats */
  {
	  LLVMValueRef arg1=LLVMBuildSIToFP(Builder,$1,LLVMFloatType(),"");
	  LLVMValueRef arg2=LLVMBuildSIToFP(Builder,$3,LLVMFloatType(),"");
	  $$=LLVMBuildFRem(Builder,arg1,arg2,"");
  }
}
;

cast_expression:          unary_expression
{ $$ = $1; }
;

lhs_expression:           ID 
{
  int isArg=0;
  /* Get the value of ID from hash map */
  LLVMValueRef val = symbol_find($1,&isArg);
  $$ = val;
}
                        | STAR ID
{
	/* Get the value of ID from hash map */
  LLVMValueRef val = symbol_find($2,NULL);
  /* The value will be stored in memory location in ID. So implemented load to extract value */
	/* from the memory location */
  $$ = LLVMBuildLoad(Builder,val,"");
  
}
;

unary_expression:         postfix_expression
{
  $$ = $1;
}
                        | AMPERSAND primary_expression
{
  /* The alloca will the the value needed. It will be stored in the hash map */
  LLVMValueRef val = symbol_find(id,NULL);
  $$=val;
}
                        | STAR primary_expression
{
	/* The value will be stored in memory location in primary_expression. So implemented load to extract value */
	/* from the memory location */
  $$ = LLVMBuildLoad(Builder,$2,"");
}
                        | MINUS unary_expression
{
  /* Implemented negation */
  $$=LLVMBuildNeg(Builder,$2,"");
}
                        | PLUS unary_expression
{
  $$ = $2;
}
                        | BITWISE_INVERT unary_expression
{
  
  /* Implemented NOT */
  $$=LLVMBuildNot(Builder,$2,"");
  
}
                        | NOT unary_expression
{
  
  LLVMValueRef zero=LLVMConstInt(LLVMTypeOf($2),0,1);
  /* for int*/
  if(LLVMTypeOf($2)==LLVMInt32Type())
  { 
	 /* Compare the expression value with zero to know the correct result*/
	  LLVMValueRef icmp=LLVMBuildICmp(Builder,LLVMIntEQ,$2,zero,"logical.neg");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMInt32Type(),"logical.neg");
  }
  /* for float */
  else
  {
	  /* Compare the expression value with zero to know the correct result*/
	  LLVMValueRef icmp=LLVMBuildFCmp(Builder,LLVMRealOEQ,$2,zero,"logical.neg");
	  $$=LLVMBuildZExt(Builder,icmp,LLVMFloatType(),"logical.neg");
  }

}
postfix_expression:       primary_expression
{
  $$ = $1;
}
;

primary_expression:       ID 
{ 
  /*find the value of the variable from the hash table */
  LLVMValueRef val = symbol_find($1,NULL);
  id=$1;
  
    $$ = LLVMBuildLoad(Builder,val,"");
 
}
                        | constant
{
  $$ = $1;
}
                        | LPAREN expression RPAREN
{
  $$ = $2;
}
;

constant:	          CONSTANT_INTEGER  
{ 
  /* Implement */
	$$=LLVMConstInt(LLVMInt32Type(),$1,0);
} 
|                         CONSTANT_FLOAT
{
	$$=LLVMConstReal(LLVMFloatType(),$1);
}
;

%%

LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params)
{
  int i;
  int size = paramlist_size(params);
  LLVMTypeRef *ParamArray = malloc(sizeof(LLVMTypeRef)*size);
  LLVMTypeRef FunType;
  LLVMBasicBlockRef BasicBlock;

  paramlist_t *tmp = params;
  /* Build type for function */
  for(i=size-1; i>=0; i--) 
    {
      ParamArray[i] = tmp->type;
      tmp = next_param(tmp);
    }
  
  FunType = LLVMFunctionType(RetType,ParamArray,size,0);

  Function = LLVMAddFunction(Module,name,FunType);
  
  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);

  tmp = params;
  for(i=size-1; i>=0; i--)
    {
      LLVMValueRef alloca = LLVMBuildAlloca(Builder,tmp->type,tmp->name);
      LLVMBuildStore(Builder,LLVMGetParam(Function,i),alloca);
      symbol_insert(tmp->name,alloca,0);
      tmp=next_param(tmp);
    }

  return Function;
}

extern int line_num; 
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;

int parser_error(const char *msg)
{
  printf("%s (%d): Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s (%d): Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror()
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void minic_abort()
{
  parser_error("Too many errors to continue.");
  exit(1);
}
