//
//  ReactComponent.m
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ReactComponent.h"
#import "PrologObject.h"
#import "Constants.h"
#import <YogaKit/YogaKit.h>

@implementation ReactComponent

-(id) initWithDOMNode:(UIView *)node
{
    self = [super init];
    if (self)
    {
        blob = [Prolog makeBlobOfType:@"react_component" withData:self];
        children = [[NSMutableArray alloc] init];
        DOMnode = node;
        orientation = VERTICAL;
        node.yoga.isEnabled = YES;
        node.yoga.flexGrow = 1;
        node.yoga.flexShrink = 1;
        node.yoga.alignItems = YGAlignCenter;
        node.yoga.justifyContent = YGJustifyCenter;

    }
    return self;
}

-(void) restyle
{
    if (orientation == VERTICAL)
        DOMnode.yoga.flexDirection = YGFlexDirectionColumn;
    else
        DOMnode.yoga.flexDirection = YGFlexDirectionRow;
    if (parent != NULL)
    {
        if (fill == NONE)
            DOMnode.yoga.flexGrow = 0;
        if (fill == HORIZONTAL_FILL || fill == BOTH)
        {
            if (parent->orientation == HORIZONTAL)
                DOMnode.yoga.flexGrow = 1;
            else
                DOMnode.yoga.alignSelf = YGAlignStretch;
        }
        if (fill == VERTICAL_FILL || fill == BOTH)
        {
            if (parent->orientation == VERTICAL)
                DOMnode.yoga.flexGrow = 1;
            else
                DOMnode.yoga.alignSelf = YGAlignStretch;
        }
    }
    [self requestLayout];
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
    int needsRestyle = 0;
    if (properties[@"fill"] != nil)
    {
        PrologObject* o = properties[@"fill"];
        NSString* value = [Prolog atomString:[o value]];
        if ([value isEqualToString:@"none"])
            fill = NONE;
        else if ([value isEqualToString:@"horizontal"])
            fill = HORIZONTAL_FILL;
        else if ([value isEqualToString:@"vertical"])
            fill = VERTICAL_FILL;
        else if ([value isEqualToString:@"both"])
            fill = BOTH;
        needsRestyle = 1;
    }
    if (properties[@"layout"] != nil)
    {
        PrologObject* o = properties[@"layout"];
        NSString* value = [Prolog atomString:[o value]];
        if ([value isEqualToString:@"horizontal"])
            orientation = HORIZONTAL;
        else if ([value isEqualToString:@"vertical"])
            orientation = VERTICAL;
        needsRestyle = 1;
    }
    if (needsRestyle)
        [self restyle];
}


-(word)blob
{
    return blob;
}

-(void)setParent:(ReactComponent *)p
{
    parent = p;
}

-(ReactComponent*)parent
{
    return parent;
}

-(void)appendChild:(ReactComponent *)child
{
    [child setParent:self];
    [children addObject:child];
    dispatch_async(dispatch_get_main_queue(),
                   ^{
                       [DOMnode addSubview:[child getDOMNode]];
                       [self requestLayout];
                    });
}

-(void)requestLayout
{
    // FIXME: This is expensive if there are a lot of changes!
    UIView* root = [DOMnode.window.subviews objectAtIndex:0];
    if (root.yoga.isEnabled)
        [root.yoga applyLayout];
}

-(void)removeChild:(ReactComponent *)child
{
    [child setParent:nil];
    [children removeObject:child];
    dispatch_async(dispatch_get_main_queue(),
                   ^{
                       [[child getDOMNode] removeFromSuperview];
                       [self requestLayout];
                   });

}

-(void)freeComponent
{
    for (ReactComponent* c in children)
        [c freeComponent];
}

-(NSArray*)getChildren
{
    return children;
}

+(word)serialize:(NSDictionary *)data
{
    word result = [Constants emptyListAtom];
    for (NSString* key in data)
        result = [Prolog makeCompoundFrom:[Constants listFunctor], [Prolog makeCompoundFrom:[Constants equalsFunctor], [Prolog makeAtomFrom:key], [((PrologObject*)[data valueForKey:key]) value]], result];
    return result;
}

@end
