//
//  ReactComponent.h
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "Prolog.h"

@class ReactWidget;

@interface ReactComponent : UIView
{
    UIView* DOMnode;
    ReactWidget* owner;
    ReactComponent* parent;
    word blob;
    NSMutableArray* children;
    enum {HORIZONTAL, VERTICAL} orientation;
    enum {HORIZONTAL_FILL, VERTICAL_FILL, NONE, BOTH} fill;
}
-(id)initWithDOMNode:(UIView*)node;
-(void)restyle;
-(void)setOwnerDocument:(ReactWidget*)widget;
-(UIView*)getDOMNode;
-(void)setProperties:(NSDictionary*)properties;
-(word)blob;
-(void)appendChild:(ReactComponent*)child;
-(void)removeChild:(ReactComponent*)child;
-(void)setParent:(ReactComponent*)parent;
-(NSMutableArray*)getChildren;
-(ReactComponent*)parent;
-(void)freeComponent;
-(void)requestLayout;

@end
