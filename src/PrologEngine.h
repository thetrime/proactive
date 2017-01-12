//
//  PrologEngine.h
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "CodeChangeListener.h"
#import "PrologState.h"
#import "ReactWidget.h"
#import "Prolog.h"
#import "PrologEnvironment.h"


@interface PrologEngine : NSObject
{
    NSString* baseURL;
    NSString* listenURL;
    NSString* goalURL;
    NSString* componentURL;
    PrologEnvironment* env;
}
-(id)configureWithUrl:(NSString*)url rootElementId:(NSString*)rootElementId thenCall:(void(^)(int))callback;;
-(void)listenForCodeChangesIn:(NSString*)componentId andNotify:(NSObject<CodeChangeListener>*)listener;
-(void)getInitialStateFor:(NSString*)elementId withProps:(PrologState*)props thenCall:(void(^)(PrologState*))callback;
-(void)renderWidget:(ReactWidget*)widget withState:(PrologState*)state props:(PrologState*)props thenCall:(void(^)(word))callback;
-(void)createElementFromVDOM:(word)vDOM forWidget:(ReactWidget*)widget thenCall:(void(^)(ReactComponent*))callback;
-(void)makeThenCall:(void(^)(int))callback;
@end
