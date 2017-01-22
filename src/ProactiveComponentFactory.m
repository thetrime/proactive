//
//  ProactiveComponentFactory.m
//  proactive
//
//  Created by Matt Lilley on 7/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ProactiveComponentFactory.h"
#import "Panel.h"
#import "Label.h"
#import "Button.h"
#import "Field.h"
#import "Table.h"
#import "Row.h"


@implementation ProactiveComponentFactory
+(ReactComponent*)createElementOfType:(NSString*)type withContext:(ReactComponent*)context
{
    if ([type isEqualToString:@"Panel"])
        return [[Panel alloc] init];
    if ([type isEqualToString:@"Button"])
        return [[Button alloc] init];
    if ([type isEqualToString:@"Label"])
        return [[Label alloc] init];
    if ([type isEqualToString:@"Field"])
        return [[Field alloc] init];
    if ([type isEqualToString:@"Table"])
        return [[Table alloc] init];
    if ([type isEqualToString:@"Row"])
        return [[Row alloc] init];
    // Missing: List, ListItem, Title, TableHeader, TabbedPane, Tab, ComboBox, ComboItem
    NSLog(@"Not implemented: %@", type);
    assert(0);
    return nil;
}
@end
