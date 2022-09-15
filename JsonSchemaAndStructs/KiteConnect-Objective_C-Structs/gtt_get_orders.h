// To parse this JSON:
//
//   NSError *error;
//   QTGttGetOrders *gttGetOrders = [QTGttGetOrders fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTGttGetOrders;
@class QTDefinitions;
@class QTCondition;
@class QTConditionProperties;
@class QTExchange;
@class QTType;
@class QTTriggerValues;
@class QTDatum;
@class QTDatumProperties;
@class QTConditionClass;
@class QTCreatedAt;
@class QTMeta;
@class QTAnyOf;
@class QTOrders;
@class QTGttGetOrdersClass;
@class QTGttGetOrdersProperties;
@class QTMetaClass;
@class QTOrder;
@class QTOrderProperties;
@class QTOrderResult;
@class QTOrderResultProperties;
@class QTResult;
@class QTResultProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)null;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTGttGetOrders : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTCondition *condition;
@property (nonatomic, strong) QTDatum *datum;
@property (nonatomic, strong) QTGttGetOrdersClass *gttGetOrders;
@property (nonatomic, strong) QTMetaClass *meta;
@property (nonatomic, strong) QTOrder *order;
@property (nonatomic, strong) QTOrderResult *orderResult;
@property (nonatomic, strong) QTResult *result;
@end

@interface QTCondition : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTConditionProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTConditionProperties : NSObject
@property (nonatomic, strong) QTExchange *exchange;
@property (nonatomic, strong) QTExchange *instrumentToken;
@property (nonatomic, strong) QTExchange *lastPrice;
@property (nonatomic, strong) QTExchange *tradingsymbol;
@property (nonatomic, strong) QTTriggerValues *triggerValues;
@end

@interface QTExchange : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTTriggerValues : NSObject
@property (nonatomic, strong) QTExchange *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatum : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDatumProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatumProperties : NSObject
@property (nonatomic, strong) QTConditionClass *condition;
@property (nonatomic, strong) QTCreatedAt *createdAt;
@property (nonatomic, strong) QTCreatedAt *expiresAt;
@property (nonatomic, strong) QTExchange *identifier;
@property (nonatomic, strong) QTMeta *meta;
@property (nonatomic, strong) QTOrders *orders;
@property (nonatomic, strong) QTExchange *parentTrigger;
@property (nonatomic, strong) QTExchange *status;
@property (nonatomic, strong) QTExchange *type;
@property (nonatomic, strong) QTCreatedAt *updatedAt;
@property (nonatomic, strong) QTExchange *userID;
@end

@interface QTConditionClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTCreatedAt : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTMeta : NSObject
@property (nonatomic, copy) NSArray<QTAnyOf *> *anyOf;
@end

@interface QTAnyOf : NSObject
@property (nonatomic, nullable, copy)   NSString *ref;
@property (nonatomic, nullable, assign) QTType *type;
@end

@interface QTOrders : NSObject
@property (nonatomic, strong) QTConditionClass *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTGttGetOrdersClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTGttGetOrdersProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTGttGetOrdersProperties : NSObject
@property (nonatomic, strong) QTOrders *data;
@property (nonatomic, strong) QTExchange *status;
@end

@interface QTMetaClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrder : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOrderProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrderProperties : NSObject
@property (nonatomic, strong) QTExchange *exchange;
@property (nonatomic, strong) QTExchange *orderType;
@property (nonatomic, strong) QTExchange *price;
@property (nonatomic, strong) QTExchange *product;
@property (nonatomic, strong) QTExchange *quantity;
@property (nonatomic, strong) QTMeta *result;
@property (nonatomic, strong) QTExchange *tradingsymbol;
@property (nonatomic, strong) QTExchange *transactionType;
@end

@interface QTOrderResult : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOrderResultProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrderResultProperties : NSObject
@property (nonatomic, strong) QTExchange *orderID;
@property (nonatomic, strong) QTExchange *rejectionReason;
@property (nonatomic, strong) QTExchange *status;
@end

@interface QTResult : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTResultProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTResultProperties : NSObject
@property (nonatomic, strong) QTExchange *accountID;
@property (nonatomic, strong) QTExchange *exchange;
@property (nonatomic, strong) QTExchange *meta;
@property (nonatomic, strong) QTConditionClass *orderResult;
@property (nonatomic, strong) QTExchange *orderType;
@property (nonatomic, strong) QTExchange *price;
@property (nonatomic, strong) QTExchange *product;
@property (nonatomic, strong) QTExchange *quantity;
@property (nonatomic, strong) QTCreatedAt *timestamp;
@property (nonatomic, strong) QTExchange *tradingsymbol;
@property (nonatomic, strong) QTExchange *transactionType;
@property (nonatomic, strong) QTExchange *triggeredAt;
@property (nonatomic, strong) QTExchange *validity;
@end

NS_ASSUME_NONNULL_END
