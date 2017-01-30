//
//  PrologEnvironment.h
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Prolog.h"

@interface PrologEnvironment : NSObject
{
    NSMutableArray* stack;
}
-(id)init;
-(void)pushExecutionState:(word)w;
-(void)popExecutionState;
-(word)peekExecutionState;
@end
