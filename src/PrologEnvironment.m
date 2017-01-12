//
//  PrologEnvironment.m
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "PrologEnvironment.h"
#import "Prolog.h"
#import "PrologObject.h"

@implementation PrologEnvironment
-(id)init
{
    self = [super init];
    if (self)
    {
        stack = [[NSMutableArray alloc] init];
    }
    return self;
}

-(void)pushExecutionState:(word) w
{
    [stack addObject:[PrologObject intern:w]];
}

-(void)popExecutionState
{
    [stack removeLastObject];
}

-(word)peekExecutionState
{
    return [(PrologObject*)[stack lastObject] value];
}


@end
