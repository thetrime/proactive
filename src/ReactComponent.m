//
//  ReactComponent.m
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ReactComponent.h"

@implementation ReactComponent

-(id) initWithDOMNode:(UIView *)node
{
    self = [super init];
    if (self)
    {
        blob = [Prolog makeBlobOfType:@"react_component" withData:self];
        DOMnode = node;
    }
    return self;
}

-(void) restyle
{
    // FIXME: implement
}

-(void)setOwnerDocument:(ReactWidget *)widget
{
    owner = widget;
}

-(UIView*)getDOMNode
{
    return DOMnode;
}

-(void)setProperties:(NSDictionary*)properties;
{
    // FIXME: Implement
}

-(word)blob
{
    return blob;
}

-(void)setParent:(ReactComponent *)p
{
    parent = p;
}

-(void)appendChild:(ReactComponent *)child
{
    [child setParent:self];
    [children addObject:child];
    dispatch_async(dispatch_get_main_queue(),
                   ^{
                       [DOMnode addSubview:[child getDOMNode]];
                   });
}

@end
