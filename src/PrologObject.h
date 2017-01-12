//
//  PrologObject.h
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Prolog.h"

@interface PrologObject : NSObject
{
    word value;
}
+(PrologObject*)intern:(word)w;
-(word)value;
@end
