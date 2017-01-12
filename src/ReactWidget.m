//
//  ReactWidget.m
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ReactWidget.h"
#import "PrologEngine.h"

@implementation ReactWidget
-(id)initWithParent:(ReactWidget*)p engine:(PrologEngine*)_engine elementId:(NSString*)_rootElementId props:(PrologState*)initialProps
{
    self = [super initWithDOMNode:nil];
    engine = _engine;
    elementId = _rootElementId;
    props = initialProps;
    owner = p;
    
    [self setProperties:[props getProperties]];
    [engine listenForCodeChangesIn:elementId andNotify:self];
    [engine getInitialStateFor:elementId withProps:props thenCall:^(PrologState* _state)
             {
                 state = _state;
                 [engine renderWidget:self withState:state props:props thenCall:^(word v)
                  {
                      vDOM = v;
                      [engine createElementFromVDOM:vDOM forWidget:self thenCall:^(ReactComponent* _component)
                      {
                          internalComponent = _component;
                          [internalComponent setOwnerDocument:self];
                          [internalComponent restyle];
                      }];
                  }];
             }];
    return self;
}

-(void)codeChanged
{
    // FIXME: Implement
    NSLog(@"Code changed. Should reload");
}

-(UIView*)getDOMNode
{
    if (internalComponent == nil)
        return nil;
    return [internalComponent getDOMNode];
}

-(NSString*)elementId
{
    return elementId;
}

@end
