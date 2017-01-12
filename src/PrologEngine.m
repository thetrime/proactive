//
//  PrologEngine.m
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "PrologEngine.h"
#import "Prolog.h"
#import "Constants.h"
#import "PrologEnvironment.h"
#import "ProactiveForeign.h"


@implementation PrologEngine

-(word)makeGoal:(word)goal inModule:(NSString*)module
{
    return [Prolog makeCompoundFrom:[Constants crossModuleFunctor], [Prolog makeAtomFrom:module], goal];
}

-(PrologEngine*) configureWithUrl:(NSString*)url rootElementId:(NSString*)rootElementId thenCall:(void (^)(int))callback
{
    [ProactiveForeign installForeignPredicates];
    env = [[PrologEnvironment alloc] init];
    baseURL = url;
    if ([[baseURL substringToIndex:5] isEqualToString:@"https"])
    {
        goalURL = [NSString stringWithFormat:@"wss%@/goal", [baseURL substringFromIndex:5]];
        listenURL = [NSString stringWithFormat:@"wss%@/listen", [baseURL substringFromIndex:5]];
    }
    else
    {
        goalURL = [NSString stringWithFormat:@"ws%@/goal", [baseURL substringFromIndex:4]];
        listenURL = [NSString stringWithFormat:@"ws%@/listen", [baseURL substringFromIndex:4]];
    }
    componentURL = [NSString stringWithFormat:@"%@/component/%@", baseURL, rootElementId];
    // FIXME: start code listener
    [self makeThenCall:callback];
    return self;
}

-(void)makeThenCall:(void(^)(int))callback
{
    [Prolog consultFile:[[[NSBundle bundleForClass:[self class]] resourcePath] stringByAppendingPathComponent:@"boilerplate.pl"]];
    [Prolog consultFile:[[[NSBundle bundleForClass:[self class]] resourcePath] stringByAppendingPathComponent:@"vdiff.pl"]];
    [Prolog consultURL:componentURL thenCall:callback];
}

-(void)listenForCodeChangesIn:(NSString *)componentId andNotify:(NSObject<CodeChangeListener> *)listener
{
    // FIXME: Implement
}

-(void)getInitialStateFor:(NSString *)elementId withProps:(PrologState *)props thenCall:(void (^)(PrologState*))callback
{
    if (![Prolog module:elementId containsPredicate:[Constants getInitialStateFunctor]])
    {
        callback([PrologState emptyState]);
    }
    word result = [Prolog makeVariable];
    word goal = [self makeGoal:[Prolog makeCompoundFrom:[Constants getInitialStateFunctor], [props blob], result] inModule:elementId];
    ExecutionState* state = [Prolog saveState];
    [Prolog executeGoal:goal inEnvironment:env thenCall:^(RC rc)
            {
                 if (rc == SUCCESS)
                 {
                     PrologState* newState = [[PrologState alloc] initWithTerm:[Prolog deref:result]];
                     [Prolog restoreState:state];
                     callback(newState);
                 }
                 else
                 {
                     [Prolog restoreState:state];
                     callback([PrologState emptyState]);
                 }
             }];
}

-(void)renderWidget:(ReactWidget *)widget withState:(PrologState *)state props:(PrologState *)props thenCall:(void (^)(word))callback
{
    word vDOM = [Prolog makeVariable];
    word goal = [self makeGoal:[Prolog makeCompoundFrom:[Constants renderFunctor], [state blob], [props blob], vDOM] inModule:[widget elementId]];
    ExecutionState* saved = [Prolog saveState];
    [env pushExecutionState:[widget blob]];
    [Prolog executeGoal:goal inEnvironment:env thenCall:^(RC rc)
     {
         [env popExecutionState];
         if (rc == SUCCESS || rc == SUCCESS_WITH_CHOICES)
         {
             // Expand child objects
             word expandedDOM = [Prolog makeVariable];
             // [Prolog cut];
             [Prolog executeGoal:[Prolog makeCompoundFrom:[Constants expandChildrenFunctor], vDOM, expandedDOM] inEnvironment:env thenCall:^(RC rc2)
              {
                  word localDOM = [Prolog makeLocal:expandedDOM];
                  [Prolog restoreState:saved];
                  if (rc2 == SUCCESS || rc2 == SUCCESS_WITH_CHOICES)
                      callback(localDOM);
                  else
                  {
                      word ex = [Prolog currentException];
                      if (ex != 0)
                      {
                          // Exception raised
                          NSLog(@"Exception: %@", [Prolog formatTerm:ex withQuotes:NO]);
                      }
                      else
                      {
                          // Failed
                          NSLog(@"Failed");
                      }
                      callback([Constants emptyListAtom]);
                  }
              }];
         }
         else
         {
             [Prolog restoreState:saved];
             word ex = [Prolog currentException];
             if (ex != 0)
             {
                 NSLog(@"Exception: %@", [Prolog formatTerm:ex withQuotes:NO]);
                 // Exception raised
             }
             else
             {
                 NSLog(@"Failed");
                 // Failed
             }
             callback([Constants emptyListAtom]);
             
         }
     }];
    
    
}

-(void)createElementFromVDOM:(word)vDOM forWidget:(ReactWidget *)widget thenCall:(void (^)(ReactComponent*))callback
{
    word result = [Prolog makeVariable];
    word renderOptions = [Prolog makeCompoundFrom:[Constants listFunctor], [Prolog makeCompoundFrom:[Constants documentFunctor], [widget blob]], [Constants emptyListAtom]];
    word goal = [Prolog makeCompoundFrom:[Constants createElementFromVdomFunctor], renderOptions, vDOM, result];
    ExecutionState* saved = [Prolog saveState];
    [Prolog executeGoal:goal inEnvironment:env thenCall:^(RC rc)
     {
         id element = nil;
         if (rc == SUCCESS || rc == SUCCESS_WITH_CHOICES)
         {
             element = [Prolog blobData:result forType:@"react_component"];
         }
         else
         {
             word ex = [Prolog currentException];
             if (ex != 0)
             {
                 NSLog(@"Exception: %@", [Prolog formatTerm:ex withQuotes:NO]);
                 // Exception raised
             }
             else
             {
                 NSLog(@"Failed");
                 // Failed
             }
         }
         callback(element);
         [Prolog restoreState:saved];
     }];    
}

@end
