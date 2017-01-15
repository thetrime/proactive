//
//  Prolog.h
//  An Objective C wrapper for Proscript
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#include <Proscript/proscript2.h>

//typedef enum {SUCCESS=1, FAIL=0} RC;
typedef void ExecutionState;
typedef int (*foreign_call_t)();

@interface Prolog : NSObject
+(void)initialize;
+(word)localTermFromString:(NSString*)string;
+(bool)module:(NSString*)module containsPredicate:(word)indicator;
+(word)makeAtomFrom:(NSString*)string;
+(word)makeVariable;
+(word)deref:(word)term;
+(word)makeCompoundFrom:(word)functor, ...;
+(word)makeFunctor:(word)name withArity:(int)arity;
+(word)makeBlobOfType:(NSString*)type withData:(id)data;
+(Choicepoint)saveState;
+(void)restoreState:(Choicepoint)state;
+(void)executeGoal:(word)goal inEnvironment:(id)env thenCall:(void(^)(RC))callback;
+(int)consultFile:(NSString*)filename;
+(void)consultURL:(NSString*)url thenCall:(void(^)(int))callback;
+(bool)isVariable:(word)w;
+(bool)isAtom:(word)w;
+(bool)isCompound:(word)w;
+(bool)isCompound:(word)w withFunctor:(word)w;
+(word)arg:(int)i ofTerm:(word)w;
+(const char*)atomChars:(word)w;
+(NSString*)atomString:(word)w;
+(void)freeLocal:(word)w;
+(bool)isBlob:(word)w ofType:(NSString*)type;
+(id)blobData:(word)w forType:(NSString*)type;
+(word)makeLocal:(word)w;
+(word)currentException;
+(NSString*)formatTerm:(word)w withQuotes:(bool)withQuotes;
+(NSString*)formatTerm:(word)w;
+(void)registerForeignPredicate:(word)functor inModule:(NSString*)module as:(foreign_call_t)func withOptions:(int)flags;
+(void)registerForeignPredicate:(word)functor inModule:(NSString*)module as:(foreign_call_t)func;
+(int)unify:(word)a and:(word)b;
+(int)setException:(word)e;
+(int)typeError:(word)a whenExpecting:(word)b;
+(word)makeNull;
+(void)releaseBlob:(word)b ofType:(NSString*)type;
@end
