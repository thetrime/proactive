//
//  ReactWidget.h
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ReactComponent.h"
#import "PrologState.h"
#import "CodeChangeListener.h"

@class PrologEngine;
@interface ReactWidget : ReactComponent<CodeChangeListener>
{
    PrologEngine* engine;
    NSString* elementId;
    PrologState* props;
    PrologState* state;
    word vDOM;
    ReactComponent* internalComponent;
    NSString* identifier;
}
-(id)initWithParent:(ReactComponent*)parent engine:(PrologEngine*)engine elementId:(NSString*)rootElementId props:(PrologState*)initialProps;
-(NSString*)elementId;
-(void)codeChanged;
-(void)reRenderThenCall:(void (^)(ReactWidget*))callback;
-(void)triggerEvent:(word)handler withData:(word)w thenCall:(void(^)(int))callback;
-(void)setState:(PrologState*)state thenCall:(void(^)(int))callback;
-(PrologState*)state;
-(PrologState*)props;
-(NSString*)elementId;

@end
