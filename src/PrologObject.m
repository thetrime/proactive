//
//  PrologObject.m
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "PrologObject.h"

@implementation PrologObject
+(PrologObject*)intern:(word)w
{
    PrologObject* o = [[PrologObject alloc] init];
    o->value = w;
    return o;
}
-(word)value
{
    return value;
}
@end
